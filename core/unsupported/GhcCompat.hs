-- |
--
--  __NB__: This module is designed to work as backward-compatibly as possible,
--          so it doesn’t use package-qualified imports, Safe Haskell, etc.
module GhcCompat
  ( plugin,
  )
where

import GhcPlugins (Plugin, defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin
