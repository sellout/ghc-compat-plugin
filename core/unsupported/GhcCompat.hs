{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- A no-op version of the "GhcCompat" plugin to load on GHC’s that the actual
-- plugin doesn’t yet compile on. This prevents users from having to
-- conditionalize their use of the plugin.
--
-- __NB__: This module must compile for GHC 7.2 (the first version that
--         introduces plugins) through the version before the “supported”
--         "GhcCompat" module (which is currently GHC 7.10).
module GhcCompat
  ( plugin,
  )
where

import "ghc" GhcPlugins (Plugin, defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin
