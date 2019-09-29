module StackDeploy.Stack
  ( finalMessage
  , getExistingStack
  , getOutput
  , getStackId
  , perform
  , printEvent
  , stackNames
  )
where

import Control.Exception.Base (AssertionFailed(AssertionFailed))
import Control.Lens (Lens', set, view)
import Control.Monad ((<=<))
import Control.Monad.Catch (catchIf, throwM)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Conduit.Combinators (find, map)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.String (String)
import Data.Text (Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import StackDeploy.AWS
import StackDeploy.IO
import StackDeploy.Prelude
import StackDeploy.Template
import StackDeploy.Types
import StackDeploy.Wait

import qualified Data.Foldable                             as Foldable
import qualified Data.Text                                 as Text
import qualified Data.Text.Encoding                        as Text
import qualified Network.AWS                               as AWS
import qualified Network.AWS.CloudFormation.CreateStack    as CF
import qualified Network.AWS.CloudFormation.DeleteStack    as CF
import qualified Network.AWS.CloudFormation.DescribeStacks as CF
import qualified Network.AWS.CloudFormation.Types          as CF
import qualified Network.AWS.CloudFormation.UpdateStack    as CF
import qualified Stratosphere

data OperationFields a = OperationFields
  { tokenField        :: Lens' a (Maybe Text)
  , capabilitiesField :: Lens' a [CF.Capability]
  , parametersField   :: Lens' a [CF.Parameter]
  , roleARNField      :: Lens' a (Maybe Text)
  , templateBodyField :: Lens' a (Maybe Text)
  }

perform
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Operation
  -> m RemoteOperationResult
perform = \case
  (OpCreate name instanceSpec template) ->
    successCallback instanceSpec =<<
      create name instanceSpec template
  (OpDelete stackId) ->
    runStackId stackId delete
  (OpUpdate stackId instanceSpec template) ->
    successCallback instanceSpec =<<
      runStackId stackId (update instanceSpec template)
  where
    runStackId
      :: Id
      -> (RemoteOperation -> m RemoteOperationResult)
      -> m RemoteOperationResult
    runStackId stackId action = do
      token <- newToken
      action RemoteOperation{..}

    create :: Name -> InstanceSpec -> Stratosphere.Template -> m RemoteOperationResult
    create name instanceSpec@InstanceSpec{..} template = do
      void prepareSync
      token   <- newToken
      stackId <- getId =<< doCreate token
      waitFor RemoteOperation{..}
      where
        getId :: AWS.Rs CF.CreateStack -> m Id
        getId =
          maybe
            (throwM $ AssertionFailed "Remote stack without stack id")
            (pure . Id)
          . view CF.csrsStackId

        doCreate :: Token -> m (AWS.Rs CF.CreateStack)
        doCreate token
          = AWS.send
          . configureStack template operationFields instanceSpec token
          . CF.createStack
          $ toText name

        operationFields = OperationFields
          { capabilitiesField = CF.csCapabilities
          , parametersField   = CF.csParameters
          , roleARNField      = CF.csRoleARN
          , templateBodyField = CF.csTemplateBody
          , tokenField        = CF.csClientRequestToken
          }

    delete :: RemoteOperation -> m RemoteOperationResult
    delete remoteOperation@RemoteOperation{..} =
      doDelete >> waitFor remoteOperation
      where
        doDelete :: m ()
        doDelete
          = void
          . AWS.send
          . setText CF.dsClientRequestToken token
          . CF.deleteStack $ toText stackId

    update
      :: InstanceSpec
      -> Stratosphere.Template
      -> RemoteOperation
      -> m RemoteOperationResult
    update
      instanceSpec@InstanceSpec{..}
      template
      remoteOperation@RemoteOperation{..} = do
        void prepareSync
        catchIf isNoUpdateError
          (doUpdate >> waitFor remoteOperation)
          (const $ pure RemoteOperationSuccess)
      where
        doUpdate :: m ()
        doUpdate = void
          . AWS.send
          . configureStack template operationFields instanceSpec token
          . CF.updateStack
          $ toText stackId

        isNoUpdateError
          ( AWS.ServiceError
            AWS.ServiceError'
            { _serviceCode =
               AWS.ErrorCode "ValidationError"
            , _serviceMessage =
              Just (AWS.ErrorMessage "No updates are to be performed.")
            }
          ) = True

        isNoUpdateError _ = False

        operationFields = OperationFields
          { capabilitiesField = CF.usCapabilities
          , parametersField   = CF.usParameters
          , roleARNField      = CF.usRoleARN
          , templateBodyField = CF.usTemplateBody
          , tokenField        = CF.usClientRequestToken
          }

    waitFor :: RemoteOperation -> m RemoteOperationResult
    waitFor remoteOperation = waitForAccept remoteOperation printEvent

    successCallback
      :: InstanceSpec
      -> RemoteOperationResult
      -> m RemoteOperationResult
    successCallback InstanceSpec{..} result = case result of
      RemoteOperationSuccess -> onSuccess >> pure result
      _                      -> pure result

printEvent :: forall m . MonadIO m => CF.StackEvent -> m ()
printEvent event = do
  say $ Text.unwords
    [ timestamp
    , physicalResourceId
    , logicalResourceId
    , resourceType
    , resourceStatus
    ]
  sayReason $ view CF.seResourceStatusReason event
  where
    logicalResourceId =
      fromMaybe
        "[unknown-logical-resource-id]"
        (view CF.seLogicalResourceId event)

    physicalResourceId =
      fromMaybe
        "[unknown-pysical-resource-id]"
        (view CF.sePhysicalResourceId event)

    resourceType =
      fromMaybe
        "[unknown-resource-type]"
        (view CF.seResourceType event)

    resourceStatus :: Text
    resourceStatus =
      maybe
        "[unknown-resource-type]"
        (convertText . show)
        (view CF.seResourceStatus event)

    timeFormat :: String
    timeFormat = "%Y-%m-%dT%H:%M:%S"

    timestamp :: Text
    timestamp
      = convertText
      . formatTime defaultTimeLocale timeFormat
      $ view CF.seTimestamp event

    sayReason :: Maybe Text -> m ()
    sayReason = maybe (pure ()) (say . ("- " <>))

getStackId :: forall m . MonadAWS m => Name -> m (Maybe Id)
getStackId = getId <=< getStack
  where
    getId :: Maybe CF.Stack -> m (Maybe Id)
    getId = maybe (pure empty) ((pure <$>) . remoteId)

    remoteId :: CF.Stack -> m Id
    remoteId value = maybe
      (throwM $ AssertionFailed "Remote stack without stack id")
      (pure . Id)
      (view CF.sStackId value)

getExistingStack :: forall m . MonadAWS m => Name -> m CF.Stack
getExistingStack name = maybe failMissingRequested pure =<< doRequest
  where
    doRequest :: m (Maybe CF.Stack)
    doRequest = AWS.liftAWS . runConduit
      $  listResource describeSpecificStack CF.dsrsStacks
      .| find ((toText name ==) . view CF.sStackName)

    failMissingRequested :: m a
    failMissingRequested
      = throwM
      . AssertionFailed
      $ "Successful request to stack " <> convertText name <> " did not return the stack"

    describeSpecificStack :: CF.DescribeStacks
    describeSpecificStack = set CF.dStackName (pure $ toText name) CF.describeStacks

getStack :: forall m . MonadAWS m => Name -> m (Maybe CF.Stack)
getStack name =
  catchIf isNotFoundError (pure <$> getExistingStack name) (const $ pure empty)
  where
    isNotFoundError
      ( AWS.ServiceError
        AWS.ServiceError'
        { _serviceCode    = AWS.ErrorCode "ValidationError"
        , _serviceMessage = Just actualMessage
        }
      )
      = actualMessage == expectedMessage

    isNotFoundError _ = False

    expectedMessage :: AWS.ErrorMessage
    expectedMessage =
      AWS.ErrorMessage $ "Stack with id " <> toText name <> " does not exist"

stackNames :: (AWSConstraint r m, MonadAWS m) => ConduitT () Name m ()
stackNames =
  listResource CF.describeStacks CF.dsrsStacks .| map (Name . view CF.sStackName)

configureStack
  :: Stratosphere.Template
  -> OperationFields a
  -> InstanceSpec
  -> Token
  -> a
  -> a
configureStack template OperationFields{..} InstanceSpec{..} token
  = set     capabilitiesField capabilities
  . set     parametersField   parameters
  . set     roleARNField      (toText <$> roleARN)
  . setText templateBodyField templateBody
  . setText tokenField        token
  where
    templateBody = Text.decodeUtf8 . toStrict $ encodeTemplate template

setText :: (Applicative f, ToText b) => Lens' a (f Text) -> b -> a -> a
setText field value = set field (pure $ toText value)

finalMessage :: RemoteOperationResult -> Text
finalMessage = \case
  RemoteOperationFailure -> "failure"
  RemoteOperationSuccess -> "succcess"

getOutput :: forall m . MonadAWS m => Name -> Text -> m Text
getOutput name key = do
  stack <- maybe
    (failStack "not found")
    pure
    =<< getStack name

  maybe
    (failStack $ "Output " <> convertText key <> " missing")
    (maybe (failStack $ "Output " <> convertText key <> " has no value") pure . view CF.oOutputValue)
    (Foldable.find ((==) (pure key) . view CF.oOutputKey) (view CF.sOutputs stack))

  where
    failStack :: Text -> m a
    failStack message
      = liftIO . fail . convertText $ "Stack: " <> convertText name <> " " <> message
