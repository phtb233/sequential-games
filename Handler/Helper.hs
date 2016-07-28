{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Helper where

import Text.Lucius
import ClassyPrelude.Yesod
import Data.Text (pack)
import Yesod

rounded :: String -> Mixin
rounded val =
        [luciusMixin|
            -webkit-border-radius: #{val};
            -moz-border-radius: #{val};
            border-radius: #{val};
        |]

transition :: String -> Mixin
transition val =
        [luciusMixin|
            -webkit-transition: #{val};
            -moz-transition: #{val};
            -o-transition: #{val};
            transition: #{val};
        |]

puzzleSize :: Int -> Mixin
puzzleSize size = 
        let minushalf = negate $ div size 2
            toEms n = show n ++ "em"
        in [luciusMixin| 
                width:  #{toEms size};
                height:  #{toEms size};
                top: 50%;
                left: 50%;
                margin-left: #{toEms minushalf};
                margin-top:  #{toEms minushalf};
            |]

center :: Mixin
center =
        [luciusMixin|
            margin-left: auto;
            margin-right: auto;
        |]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (this,rest) = splitAt n xs
              in this : chunks n rest
