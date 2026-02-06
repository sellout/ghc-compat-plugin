{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Monoid ((<>))
import System.IO (IO)
import qualified Test.SupportGHC2021
import Prelude ()

main :: IO ()
main = pure Test.SupportGHC2021.force
