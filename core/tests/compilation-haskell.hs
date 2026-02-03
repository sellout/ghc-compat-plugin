{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Monoid ((<>))
import System.IO (IO)
import qualified Test.Min_6_0_1
import qualified Test.Min_6_10_1
import qualified Test.Min_6_12_1
import qualified Test.Min_6_8_1
import qualified Test.Min_7_0_1
import qualified Test.Min_7_2_1
import Prelude ()
#if MIN_VERSION_GLASGOW_HASKELL(7, 4, 1, 0)
import qualified Test.Min_7_4_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 6, 1, 0)
import qualified Test.Min_7_6_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 8, 1, 0)
import qualified Test.Min_7_8_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 10, 1, 0)
import qualified Test.Min_7_10_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
import qualified Test.Min_8_0_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
import qualified Test.Min_8_2_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 4, 0, 0)
import qualified Test.Min_8_4_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
import qualified Test.Min_8_6_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0)
import qualified Test.Min_8_8_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
import qualified Test.Min_8_10_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
import qualified Test.Min_9_0_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
import qualified Test.SupportGHC2021
#endif

main :: IO ()
main =
  pure $
    Test.Min_6_0_1.force
      <> Test.Min_6_8_1.force
      <> Test.Min_6_10_1.force
      <> Test.Min_6_12_1.force
      <> Test.Min_7_0_1.force
      <> Test.Min_7_2_1.force
#if MIN_VERSION_GLASGOW_HASKELL(7, 4, 1, 0)
      <> Test.Min_7_4_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 6, 1, 0)
      <> Test.Min_7_6_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 8, 1, 0)
      <> Test.Min_7_8_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(7, 10, 1, 0)
      <> Test.Min_7_10_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
      <> Test.Min_8_0_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 2, 1, 0)
      <> Test.Min_8_2_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 4, 0, 0)
      <> Test.Min_8_4_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
      <> Test.Min_8_6_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0)
      <> Test.Min_8_8_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
      <> Test.Min_8_10_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
      <> Test.Min_9_0_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
      <> Test.SupportGHC2021.force
#endif
