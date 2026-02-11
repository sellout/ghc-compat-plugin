{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This is code that should live in ghc-plugin-utils.
module GhcCompat.Utils
  ( Plugin,
    errorPrelude,
    nopPlugin,
  )
where

import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$))
import safe "base" Data.Int (Int)
import safe "base" Data.List (intercalate)
import safe "base" Data.Monoid ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Version (makeVersion, showVersion)
import safe "base" System.IO (putStr)
#if MIN_VERSION_ghc(9, 0, 1)
import "ghc" GHC.Plugins (Plugin)
import qualified "ghc" GHC.Plugins as Plugins
#else
import "ghc" GhcPlugins (Plugin)
import qualified "ghc" GhcPlugins as Plugins
#endif

-- | What to print before a diagnostic message to make a plugin report look like
--   other GHC reports. The parameters are structured so that it can be
--   partially applied with the module at the top-level, and then the
--   `Plugins.CommandLineOption`s at each phase, and finally the prefix for each
--   diagnostic.
errorPrelude ::
  -- | The name of the module that defines the @plugin@ entry-point.
  String ->
  -- | The options passed to a particular plugin phase.
  [Plugins.CommandLineOption] ->
  -- | The severity of the diagnostic. Generally “warning”, “error”, or “info”.
  String ->
  String
errorPrelude pluginModule optStrs prefix =
  "on the commandline: "
    <> prefix
    <> ": ["
    <> pluginModule
    <> " plugin] ["
    <> intercalate ", " optStrs
    <> "]\n    "

-- | This is a plugin that does nothing but report that it’s doing nothing. This
--   is a fallback for plugins where we want to avoid making users
--   conditionalize the use of the plugin.
--
--  __NB__: Where possible (GHC 8.6+), this plugin won’t trigger recompilation.
nopPlugin ::
  -- | The module name of the plugin.
  String ->
  -- | The oldest GHC version that should load the _real_ implementation, not
  --   this one.
  --
  --   For example, @[7, 10, 1]@.
  [Int] ->
  Plugins.Plugin
nopPlugin pluginModule minSupportedGhcVersion =
  Plugins.defaultPlugin
    { Plugins.installCoreToDos = \optStrs ->
        ( <$
            Plugins.liftIO
              ( putStr $
                  errorPrelude pluginModule optStrs "info"
                    <> "This plugin has no effect prior to GHC "
                    <> showVersion (makeVersion minSupportedGhcVersion)
                    <> ", so ensure it’s being tested\n    on a newer release."
              )
        )
    }
#if MIN_VERSION_ghc(8, 6, 1)
    { Plugins.pluginRecompile = Plugins.purePlugin
    }
#endif
