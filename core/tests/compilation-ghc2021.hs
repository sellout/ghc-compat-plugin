{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Monoid ((<>))
import System.IO (IO)
import qualified Test.SupportGHC2021
import Prelude ()
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
import qualified Test.SupportGHC2024
#endif

main :: IO ()
main =
  pure $
    Test.SupportGHC2021.force
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
      <> Test.SupportGHC2024.force
#endif
