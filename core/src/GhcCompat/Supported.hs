{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- | The implementation of the plugin, but this module is only loaded on
--   GHC 7.10+.
module GhcCompat.Supported
  ( plugin,

    -- * exported purely for documentation
    Opts,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Either (either)
import safe "base" Data.Foldable (concatMap, foldl')
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (intercalate, intersect)
import safe "base" Data.List.NonEmpty (nonEmpty)
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Monoid (mconcat)
import safe "base" Data.Ord ((<))
import safe "base" Data.Semigroup (sconcat, (<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (uncurry)
import safe "base" Data.Version (Version, showVersion)
import safe "base" System.Exit (die)
import safe "base" System.IO (IO, putStr)
import safe "base" Text.Show (show)
import safe "ghc-boot-th" GHC.LanguageExtensions.Type (Extension)
import safe qualified "ghc-boot-th" GHC.LanguageExtensions.Type as Extension
import safe qualified "this" GhcCompat.Supported.GhcRelease as GhcRelease
import safe "this" GhcCompat.Supported.Opts (Opts)
import safe qualified "this" GhcCompat.Supported.Opts as Opts
#if MIN_VERSION_ghc(9, 0, 0)
import "ghc" GHC.Plugins
  ( Plugin,
    defaultPlugin,
    driverPlugin,
    pluginRecompile,
  )
import qualified "ghc" GHC.Plugins as Plugins
#else
import "ghc" GhcPlugins
  ( Plugin,
    defaultPlugin,
    driverPlugin,
    pluginRecompile,
  )
import qualified "ghc" GhcPlugins as Plugins
#endif

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = \optStrs env ->
        fmap (\dflags -> env {Plugins.hsc_dflags = dflags})
          . dflagsPlugin optStrs
          $ Plugins.extractDynFlags env,
      pluginRecompile = Plugins.flagRecompile
    }

dflagsPlugin ::
  [Plugins.CommandLineOption] -> Plugins.DynFlags -> IO Plugins.DynFlags
dflagsPlugin optStrs dflags = do
  opts <-
    either (die . (errorPrelude optStrs "error" <>)) pure $ Opts.parse optStrs
  let minVer = Opts.minVersion opts
  maybe
    (pure ())
    ( \level ->
        maybe
          (pure ())
          ( ( case level of
                Opts.Warn -> putStr . (errorPrelude optStrs "warning" <>)
                Opts.Error -> die . (errorPrelude optStrs "error" <>)
            )
              . ( ( "You’re using the following extensions, which aren’t compatible with ‘minVersion’ ("
                      <> showVersion minVer
                      <> "):\n"
                  )
                    <>
                )
              . sconcat
              . fmap (\ext -> "  • " <> show ext <> "\n")
          )
          . nonEmpty
          $ usedIncompatibleExtensions minVer dflags
    )
    $ Opts.reportIncompatibleExtensions opts
  pure $ updateFlags minVer dflags

errorPrelude :: [Plugins.CommandLineOption] -> String -> String
errorPrelude optStrs prefix =
  "on the commandline: "
    <> prefix
    <> ": [GhcCompat plugin] ["
    <> intercalate ", " optStrs
    <> "]\n    "

updateFlags :: Version -> Plugins.DynFlags -> Plugins.DynFlags
updateFlags minVersion dflags =
  foldl' Plugins.wopt_unset dflags $ warningFlags minVersion

-- | A list of extensions incompatible with the provided version that are used
--   (regardless of `Extension.OnOff`).
--
--  __TODO__: These are extensions in the ghc 9.14.1 library that aren’t
--            documented in the manual. I think they’re ones that have been
--            “removed” – but can they still be specified? Should we report
--            their use as well (for forward compatibility)?
--
--          - `Extension.AlternativeLayoutRule`,
--          - `Extension.AlternativeLayoutRuleTransitional`,
--          - `Extension.AutoDeriveTypeable`,
--          - `Extension.JavaScriptFFI`,
--          - `Extension.ParallelArrays`,
--          - `Extension.RelaxedLayout`, and
--          - `Extension.RelaxedPolyRec`.
usedIncompatibleExtensions :: Version -> Plugins.DynFlags -> [Extension]
usedIncompatibleExtensions minVersion =
  intersect (incompatibleExtensions minVersion)
    . fmap removeSwitch
    . Plugins.extensions
  where
    removeSwitch = \case
      Plugins.Off a -> a
      Plugins.On a -> a

-- |
--
--  __NB__: Safe Haskell is managed elsewhere, but was added in GHC 7.2.1
--          (@Unsafe@ was added in 7.4.1).
_extensions :: [Extension]
_extensions =
  [ -- I think these first few have been removed, so they should always warn,
    -- for forward incompatibility.
    Extension.AlternativeLayoutRule,
    Extension.AlternativeLayoutRuleTransitional,
    Extension.AutoDeriveTypeable,
    Extension.JavaScriptFFI,
    Extension.ParallelArrays,
    Extension.RelaxedLayout,
    Extension.RelaxedPolyRec
  ]

-- | A list of /all/ extensions that are incompatible with the provided version.
incompatibleExtensions :: Version -> [Extension]
incompatibleExtensions minVersion =
  ( \ghc ->
      if minVersion < GhcRelease.version ghc
        then GhcRelease.newExtensions ghc
        else []
  )
    =<< GhcRelease.all

warningFlags :: Version -> [Plugins.WarningFlag]
warningFlags =
  mconcat $
    disableIfOlder . fmap (first GhcRelease.version) . GhcRelease.newWarnings
      <$> GhcRelease.all

whenOlder ::
  Version -> Version -> [Plugins.WarningFlag] -> [Plugins.WarningFlag]
whenOlder minVersion flagAddedVersion flags =
  if minVersion < flagAddedVersion then flags else []

disableIfOlder ::
  [(Version, [Plugins.WarningFlag])] -> Version -> [Plugins.WarningFlag]
disableIfOlder = flip $ concatMap . uncurry . whenOlder
