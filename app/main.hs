{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Prelude     (IO, putStrLn, show)
import Application (appMain)
import Settings (appPort)
import Data.String.Interpolate (i)
import Control.Monad.Logger (logInfo)
import Data.Text (pack)

main :: IO ()
main = do
    putStrLn [i|Running on http://localhost:3000...|]
    appMain
