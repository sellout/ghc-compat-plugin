module Main (main) where

import Control.Applicative (pure)
import System.IO (IO)
import qualified Test.SupportGHC2024

main :: IO ()
main = pure Test.SupportGHC2024.force
