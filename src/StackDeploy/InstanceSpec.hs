module StackDeploy.InstanceSpec
  ( InstanceSpec(..)
  , Name(..)
  , Provider
  , get
  )
where

import StackDeploy.AWS
import StackDeploy.Parameters
import StackDeploy.Prelude
import StackDeploy.Template (Template)

import qualified Network.AWS.CloudFormation.Types as CF
import qualified StackDeploy.Provider             as Provider

newtype Name = Name Text
  deriving newtype ToText
  deriving stock   Eq

newtype RoleARN = RoleARN Text
  deriving newtype ToText
  deriving stock   Eq

type Provider = Provider.Provider InstanceSpec

data InstanceSpec = InstanceSpec
  { capabilities :: [CF.Capability]
  , name         :: Name
  , onSuccess    :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , parameters   :: Parameters
  , prepareSync  :: forall m r . (AWSConstraint r m, MonadAWS m) => m ()
  , roleARN      :: Maybe RoleARN
  , template     :: Template
  }

get
  :: forall m . MonadIO m
  => Provider
  -> Name
  -> Parameters
  -> m InstanceSpec
get provider targetName userParameters = do
  instanceSpec@InstanceSpec{..} <- Provider.get "instance-spec" name provider targetName

  pure $ instanceSpec
    { parameters = expandTemplate parameters template `union` userParameters
    }
