-- GHC 6.8.1
{-# LANGUAGE CPP #-}
-- GHC 7.2.1
{-# LANGUAGE Trustworthy #-}
-- GHC 6.10
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import safe "base" Data.Char (isUpper, toLower)
import safe "base" Data.Either (either)
import safe qualified "base" Data.Foldable as Foldable
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.List (break, drop, filter, intercalate, intersect)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
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
import qualified "ghc" GHC.Data.EnumSet as EnumSet
import safe "ghc-boot-th" GHC.LanguageExtensions.Type (Extension)
import safe "this" GhcCompat.Supported.GhcRelease (GhcRelease)
import safe qualified "this" GhcCompat.Supported.GhcRelease as GhcRelease
import safe "this" GhcCompat.Supported.Opts (Opts)
import safe qualified "this" GhcCompat.Supported.Opts as Opts
#if MIN_VERSION_ghc(9, 0, 0)
import "ghc" GHC.Plugins (Plugin, defaultPlugin)
import qualified "ghc" GHC.Plugins as Plugins
#else
import "ghc" GhcPlugins (Plugin, defaultPlugin)
import qualified "ghc" GhcPlugins as Plugins
#endif

plugin :: Plugin
plugin =
  defaultPlugin
#if MIN_VERSION_ghc(9, 2, 1)
    { Plugins.driverPlugin = \optStrs env ->
        fmap (\dflags -> env {Plugins.hsc_dflags = dflags})
          . dflagsPlugin optStrs
          $ Plugins.extractDynFlags env,
      Plugins.pluginRecompile = Plugins.flagRecompile
    }
#elif MIN_VERSION_ghc(8, 10, 1)
    { Plugins.dynflagsPlugin = dflagsPlugin,
      Plugins.pluginRecompile = Plugins.flagRecompile
    }
#elif MIN_VERSION_ghc(8, 6, 1)
    { Plugins.installCoreToDos = install,
      Plugins.pluginRecompile = Plugins.purePlugin
    }
#else
    { Plugins.installCoreToDos = install
    }
#endif

#if MIN_VERSION_ghc(8, 10, 1)
dflagsPlugin ::
  [Plugins.CommandLineOption] -> Plugins.DynFlags -> IO Plugins.DynFlags
dflagsPlugin optStrs dflags = do
  opts <-
    either (die . (errorPrelude optStrs "error" <>)) pure $ Opts.parse optStrs
  warnFlags optStrs opts dflags
  warnExts optStrs opts dflags
  pure $ updateFlags (Opts.minVersion opts) dflags

updateFlags :: Version -> Plugins.DynFlags -> Plugins.DynFlags
updateFlags minVersion dflags =
  Foldable.foldl' Plugins.wopt_unset dflags $
    warningFlags minVersion GhcRelease.all
#else
install ::
  [Plugins.CommandLineOption] ->
  [Plugins.CoreToDo] ->
  Plugins.CoreM [Plugins.CoreToDo]
install optStrs todos = do
  opts <-
    Plugins.liftIO . either (die . (errorPrelude optStrs "error" <>)) pure $
      Opts.parse optStrs
  dflags <- Plugins.getDynFlags
  Plugins.liftIO $ warnFlags optStrs opts dflags
  Plugins.liftIO $ warnExts optStrs opts dflags
  pure todos
#endif

-- | If you support a GHC older than 8.10.1, we can’t disable the flags that
--   were introduced before 8.10.1, because we have no way to modify
--   `Plugins.Dynflags`, so those flags get reported like incompatible
--   extensions.
identifyProblematicFlags :: Version -> Plugins.DynFlags -> [Plugins.WarningFlag]
identifyProblematicFlags minVersion dflags =
  filter (flip Plugins.wopt dflags) . warningFlags minVersion $
    filter
      ((< GhcRelease.version GhcRelease.ghc_8_10_1) . GhcRelease.version)
      GhcRelease.all

-- | Try to print a flag the way it looks to a user.
--
-- __FIXME__: `show` on flags doesn’t display them nicely, but I don’t see
--            another way to print them.
formatFlag :: Plugins.WarningFlag -> String
formatFlag = ("-W" <>) . intercalate "-" . splitWords [] . drop 8 . show
  where
    splitWords :: [String] -> String -> [String]
    splitWords acc =
      maybe
        acc
        ( \(h :| t) ->
            uncurry splitWords . first ((acc <>) . pure . (toLower h :)) $
              break isUpper t
        )
        . nonEmpty

warnFlags :: [Plugins.CommandLineOption] -> Opts -> Plugins.DynFlags -> IO ()
warnFlags optStrs opts dflags =
  let minVer = Opts.minVersion opts
   in maybe
        (pure ())
        ( \level ->
            maybe
              (pure ())
              ( ( case level of
                    Opts.Warn -> putStr . (errorPrelude optStrs "warning" <>)
                    Opts.Error -> die . (errorPrelude optStrs "error" <>)
                )
                  . ( ( "You have the following warnings enabled, which require the use of features\n    not available in ‘minVersion’ ("
                          <> showVersion minVer
                          <> "). Unfortunately, these warnings were\n    introduced before plugins could disable warnings automatically, so it must\n    be done manually:\n"
                      )
                        <>
                    )
                  . sconcat
                  . fmap (\flag -> "  • " <> formatFlag flag <> "\n")
              )
              . nonEmpty
              $ identifyProblematicFlags minVer dflags
        )
        $ Opts.reportIncompatibleExtensions opts

warnExts :: [Plugins.CommandLineOption] -> Opts -> Plugins.DynFlags -> IO ()
warnExts optStrs opts dflags =
  let minVer = Opts.minVersion opts
   in maybe
        (pure ())
        ( \level ->
            maybe
              (pure ())
              ( ( case level of
                    Opts.Warn -> putStr . (errorPrelude optStrs "warning" <>)
                    Opts.Error -> die . (errorPrelude optStrs "error" <>)
                )
                  . ( ( "You’re using the following extensions, which aren’t compatible with\n   ‘minVersion’ ("
                          <> showVersion minVer
                          <> "):\n"
                      )
                        <>
                    )
                  . sconcat
                  -- FIXME: Most extensions have the same constructor name as
                  --        the extension name, but not all of them, so `show`
                  --        doesn’t always do the right thing.
                  . fmap (\ext -> "  • " <> show ext <> "\n")
              )
              . nonEmpty
              $ usedIncompatibleExtensions minVer dflags
        )
        $ Opts.reportIncompatibleExtensions opts

errorPrelude :: [Plugins.CommandLineOption] -> String -> String
errorPrelude optStrs prefix =
  "on the commandline: "
    <> prefix
    <> ": [GhcCompat plugin] ["
    <> intercalate ", " optStrs
    <> "]\n    "

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
  intersect (incompatibleExtensions minVersion) . EnumSet.toList . Plugins.extensionFlags

-- | A list of /all/ extensions that are incompatible with the provided version.
incompatibleExtensions :: Version -> [Extension]
incompatibleExtensions minVersion =
  ( \ghc ->
      if minVersion < GhcRelease.version ghc
        then GhcRelease.newExtensions ghc
        else []
  )
    =<< GhcRelease.all

warningFlags :: Version -> [GhcRelease] -> [Plugins.WarningFlag]
warningFlags minVersion releases =
  mconcat
    ( disableIfOlder . fmap (first GhcRelease.version) . GhcRelease.newWarnings
        <$> releases
    )
    minVersion

whenOlder ::
  Version -> Version -> [Plugins.WarningFlag] -> [Plugins.WarningFlag]
whenOlder minVersion flagAddedVersion flags =
  if minVersion < flagAddedVersion then flags else []

disableIfOlder ::
  [(Version, [Plugins.WarningFlag])] -> Version -> [Plugins.WarningFlag]
disableIfOlder = flip $ Foldable.concatMap . uncurry . whenOlder
