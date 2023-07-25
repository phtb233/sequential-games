{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Utils where

import Data.String.Interpolate (i)
import Control.Monad.Logger (logInfo, MonadLogger)
import Data.Text (pack)
import GHC.Show


logInfoHelper :: (MonadLogger m, Show a, Show b) => a -> b -> m ()
logInfoHelper functionName message = $(logInfo) (pack [i|[#{functionName}]: #{message}|])
