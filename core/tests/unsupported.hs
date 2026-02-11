{-# LANGUAGE Safe #-}

module Main (main) where

import "base" Control.Applicative (pure)
import "base" Data.Function (($))
import "base" Data.Functor (void)
import "base" System.IO (IO)
import "this" GhcCompat (plugin)

main :: IO ()
main = void $ pure plugin
