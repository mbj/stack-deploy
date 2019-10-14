module StackDeploy.InstanceSpec
  ( InstanceSpec(..)
  , Name(..)
  , Provider
  , RoleARN(..)
  , get
  , mk
  )
where

import StackDeploy.AWS
import StackDeploy.Parameters (Parameters)
import StackDeploy.Prelude
import StackDeploy.Template (Template)

import qualified Network.AWS.CloudFormation.Types as CF
import qualified StackDeploy.Parameters           as Parameters
import qualified StackDeploy.Provider             as Provider

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

newtype RoleARN = RoleARN Text
  deriving newtype ToText
  deriving stock   Eq

type Provider = Provider.Provider InstanceSpec

data InstanceSpec = InstanceSpec
  { capabilities      :: [CF.Capability]
  , name              :: Name
  , onSuccess         :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , parameters        :: Parameters
  , parametersFromEnv :: forall m r . (AWSConstraint r m, MonadAWS m) => m Parameters
  , prepareSync       :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , roleARN           :: Maybe RoleARN
  , template          :: Template
  }

get
  :: forall m r . (AWSConstraint r m, MonadAWS m)
  => Provider
  -> Name
  -> Parameters
  -> m InstanceSpec
get provider targetName userParameters = do
  instanceSpec <-
    Provider.get "instance-spec" name provider targetName

  parameterUpdates <- parametersFromEnv instanceSpec

  pure $ instanceSpec
    { parameters
      =       expandedParameters instanceSpec
      `union` parameterUpdates
      `union` userParameters
    }

  where
    expandedParameters :: InstanceSpec -> Parameters
    expandedParameters InstanceSpec{..} =
      Parameters.expandTemplate parameters template

    union = Parameters.union


mk :: Name -> Template -> InstanceSpec
mk name template = InstanceSpec
  { capabilities      = empty
  , onSuccess         = pure ()
  , parameters        = Parameters.empty
  , parametersFromEnv = pure Parameters.empty
  , prepareSync       = pure ()
  , roleARN           = empty
  , ..
  }
