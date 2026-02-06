-- | NOP test, to skip on compilers that don’t support the particular test
module Main (main) where

import Control.Applicative (pure)
import System.IO (IO)
import Prelude ()

main :: IO ()
main = pure ()
