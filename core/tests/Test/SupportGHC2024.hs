{-# LANGUAGE CPP #-}

module Test.SupportGHC2024 (force) where

import Data.Monoid ((<>))
import qualified Test.Min_9_10_1
import Prelude ()
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 1, 0)
import qualified Test.Min_9_12_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
import qualified Test.Min_9_14_1
#endif

force :: ()
force =
  Test.Min_9_10_1.force
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 1, 0)
    <> Test.Min_9_12_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
    <> Test.Min_9_14_1.force
#endif
