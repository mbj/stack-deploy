module StackDeploy.Types where

import Data.ByteString.Builder (toLazyByteString, word32HexFixed)
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word32)
import StackDeploy.AWS
import StackDeploy.Prelude
import System.Random (randomIO)

import qualified Data.Text.Encoding               as Text
import qualified Network.AWS.CloudFormation.Types as CF
import qualified Stratosphere

newtype Id = Id Text
  deriving newtype ToText

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

newtype RoleARN = RoleARN Text
  deriving newtype ToText
  deriving stock   Eq

data Operation
  = OpCreate Name InstanceSpec Stratosphere.Template
  | OpDelete Id
  | OpUpdate Id InstanceSpec Stratosphere.Template

data InstanceSpec = InstanceSpec
  { capabilities :: [CF.Capability]
  , onSuccess    :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , parameters   :: [CF.Parameter]
  , prepareSync  :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , roleARN      :: Maybe RoleARN
  }

data RemoteOperation = RemoteOperation
  { stackId   :: Id
  , token     :: Token
  }

data RemoteOperationResult = RemoteOperationFailure | RemoteOperationSuccess

newtype Token = Token Text
  deriving newtype ToText

verb :: Operation -> Text
verb = \case
  (OpCreate _name _instanceSpec _template) -> "create"
  (OpDelete _id                          ) -> "delete"
  (OpUpdate _id   _instanceSpec _template) -> "update"

newToken :: forall m . MonadIO m => m Token
newToken = Token . text <$> bytes
  where
    text (wordA, wordB, wordC) = Text.decodeUtf8 . toStrict . toLazyByteString
      $  "stack-deploy-"
      <> word32HexFixed wordA
      <> word32HexFixed wordB
      <> word32HexFixed wordC

    bytes = (,,) <$> randomWord <*> randomWord <*> randomWord

    randomWord :: m Word32
    randomWord = liftIO randomIO
