{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Utils where

import Data.String.Interpolate (i)
import Control.Monad.Logger (logInfo)
import Data.Text (pack)


logInfoHelper functionName message = $(logInfo) (pack [i|[#{functionName}]: #{message}|])
