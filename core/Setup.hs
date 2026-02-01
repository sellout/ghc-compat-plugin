-- __NB__: `custom-setup` doesn’t have any way to specify extensions or options,
--         so any we want need to be specified here.
{-# LANGUAGE Unsafe #-}

module Main (main) where

import safe Distribution.Simple (defaultMain)

main :: IO ()
main = defaultMain
