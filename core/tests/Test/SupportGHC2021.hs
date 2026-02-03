{-# LANGUAGE CPP #-}

module Test.SupportGHC2021 (force) where

import Data.Monoid ((<>))
import qualified Test.Min_9_2_1
import Prelude ()
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 4, 0)
import qualified Test.Min_9_2_4
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
import qualified Test.Min_9_6_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
import qualified Test.Min_9_8_1
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
import qualified Test.SupportGHC2024
#endif

force :: ()
force =
  Test.Min_9_2_1.force
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 4, 0)
    <> Test.Min_9_2_4.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
    <> Test.Min_9_6_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
    <> Test.Min_9_8_1.force
#endif
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
    <> Test.SupportGHC2024.force
#endif
