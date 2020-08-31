module StackDeploy.IO (say) where

import StackDeploy.Prelude

import qualified Data.Text.IO as Text

say :: (MonadIO m, Conversion Text a) => a -> m ()
say = liftIO . Text.putStrLn . toText
