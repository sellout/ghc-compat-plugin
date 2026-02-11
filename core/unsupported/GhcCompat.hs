{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- A no-op version of the "GhcCompat" plugin to load on GHC’s that the actual
-- plugin doesn’t yet compile on. This prevents users from having to
-- conditionalize their use of the plugin.
module GhcCompat
  ( plugin,
  )
where

import "this" GhcCompat.Utils (Plugin, nopPlugin)

-- | The entry-point for the GHC plugin. This is used by passing
--   [@-fplugin=GhcCompat@](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#ghc-flag-fplugin-module)
--   to GHC.
--
--  __NB__: This plugin has no effect prior to GHC 9.6.
plugin :: Plugin
plugin = nopPlugin "GhcCompat" [7, 10, 1]
