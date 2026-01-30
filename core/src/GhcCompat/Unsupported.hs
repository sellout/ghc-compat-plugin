{-# OPTIONS_GHC -Wno-safe #-}

-- |
--
--  __NB__: This module is designed to work as backward-compatibly as possible,
--          so it doesn’t use package-qualified imports, Safe Haskell, etc.
module GhcCompat.Unsupported
  ( plugin,
  )
where

import GhcPlugins (Plugin, defaultPlugin, pluginRecompile, purePlugin)

plugin :: Plugin
plugin = defaultPlugin {pluginRecompile = purePlugin}
