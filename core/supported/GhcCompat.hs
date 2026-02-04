-- GHC 6.8.1
{-# LANGUAGE CPP #-}
-- GHC 7.2.1
{-# LANGUAGE Trustworthy #-}
-- GHC 6.10
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- The implementation of the plugin, but this module is only loaded on GHC 7.10+
-- currently.
module GhcCompat
  ( plugin,
    -- exported purely for documentation
    Opts (..),
    ReportLevel (..),
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (Bool, not, (&&))
import safe qualified "base" Data.Bool as Bool
import safe "base" Data.Char (isUpper, toLower)
import safe "base" Data.Either (either)
import safe qualified "base" Data.Foldable as Foldable
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe qualified "base" Data.List as List
import safe qualified "base" Data.Maybe as Maybe
import safe "base" Data.Monoid (mconcat, (<>))
import safe "base" Data.Ord ((<))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, snd, uncurry)
import safe "base" Data.Version (Version, showVersion)
import safe "base" System.Exit (die)
import safe "base" System.IO (IO, putStr)
import safe "base" Text.Show (show)
import safe "this" GhcCompat.GhcRelease (GhcRelease)
import safe qualified "this" GhcCompat.GhcRelease as GhcRelease
import safe "this" GhcCompat.Opts
  ( Opts (Opts),
    ReportLevel (Error, Warn),
    correctOptionOrder,
  )
import safe qualified "this" GhcCompat.Opts as Opts
#if MIN_VERSION_ghc(8, 2, 1)
import safe "base" Data.Eq ((==))
#endif
#if MIN_VERSION_ghc(9, 0, 0)
import "ghc" GHC.Plugins (Plugin, defaultPlugin)
import qualified "ghc" GHC.Plugins as Plugins
#else
import "ghc" GhcPlugins (Plugin, defaultPlugin)
import qualified "ghc" GhcPlugins as Plugins
#endif

-- | The entry-point for the GHC plugin. This is used by passing
--   [@-fplugin=GhcCompat@](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#ghc-flag-fplugin-module)
--   to GHC.
plugin :: Plugin
plugin =
  defaultPlugin
#if MIN_VERSION_ghc(9, 2, 1)
    { Plugins.driverPlugin = \optStrs env ->
        fmap (\dflags -> env {Plugins.hsc_dflags = dflags})
          . dflagsPlugin (correctOptionOrder optStrs)
          $ Plugins.hsc_dflags env,
      Plugins.pluginRecompile = Plugins.flagRecompile
    }
#elif MIN_VERSION_ghc(8, 10, 1)
    { Plugins.dynflagsPlugin = dflagsPlugin . correctOptionOrder,
      Plugins.pluginRecompile = Plugins.flagRecompile
    }
#elif MIN_VERSION_ghc(8, 6, 1)
    { Plugins.installCoreToDos = install . correctOptionOrder,
      Plugins.pluginRecompile = Plugins.purePlugin
    }
#else
    { Plugins.installCoreToDos = install . correctOptionOrder
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
  List.filter (`Plugins.wopt` dflags) . warningFlags minVersion $
    List.filter
      ((< GhcRelease.version GhcRelease.ghc_8_10_1) . GhcRelease.version)
      GhcRelease.all

-- | Try to print a flag the way it looks to a user.
--
--  __FIXME__: `show` on flags doesn’t display them nicely, but I don’t see
--             another way to print them.
--
--  __TODO__: Print out what the user should add to their Cabal file to avoid
--            these warnings (including using `-fno-warn-` for flags added
--            before GHC 8.0).
formatFlag :: Plugins.WarningFlag -> String
formatFlag =
  ("-W" <>) . List.intercalate "-" . splitWords [] . List.drop 8 . show
  where
    splitWords :: [String] -> String -> [String]
    splitWords acc =
      Maybe.maybe
        acc
        ( \(h, t) ->
            uncurry splitWords . first ((acc <>) . pure . (toLower h :)) $
              List.break isUpper t
        )
        . List.uncons

warnFlags :: [Plugins.CommandLineOption] -> Opts -> Plugins.DynFlags -> IO ()
warnFlags optStrs opts dflags =
  let minVer = Opts.minVersion opts
   in Maybe.maybe
        (pure ())
        ( \level ->
            Maybe.maybe
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
                  . mconcat
                  . fmap (\flag -> "  • " <> formatFlag flag <> "\n")
                  . uncurry (:)
              )
              . List.uncons
              $ identifyProblematicFlags minVer dflags
        )
        $ Opts.reportIncompatibleExtensions opts

warnExts :: [Plugins.CommandLineOption] -> Opts -> Plugins.DynFlags -> IO ()
warnExts optStrs opts dflags =
  let minVer = Opts.minVersion opts
   in Maybe.maybe
        (pure ())
        ( \level ->
            Maybe.maybe
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
                  . mconcat
                  -- FIXME: Most extensions have the same constructor name as
                  --        the extension name, but not all of them, so `show`
                  --        doesn’t always do the right thing.
                  . fmap (\ext -> "  • " <> show ext <> "\n")
                  . uncurry (:)
              )
              . List.uncons
              $ usedIncompatibleExtensions minVer dflags
        )
        $ Opts.reportIncompatibleExtensions opts

errorPrelude :: [Plugins.CommandLineOption] -> String -> String
errorPrelude optStrs prefix =
  "on the commandline: "
    <> prefix
    <> ": [GhcCompat plugin] ["
    <> List.intercalate ", " optStrs
    <> "]\n    "

-- | A list of extensions incompatible with the provided version that are used
--   (regardless of `Extension.OnOff`).
--
--  __NB__: Prior to GHC 9.6.1, there doesn’t seem to be a way to get all of the
--          extensions regardless of whether they’re off or on (this is helpful,
--          because even @NoFoo@ is going to fail before @Foo@ is added to the
--          compiler).
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
usedIncompatibleExtensions ::
  Version -> Plugins.DynFlags -> [GhcRelease.Extension]
#if MIN_VERSION_ghc(9, 6, 1)
usedIncompatibleExtensions minVersion dflags =
  List.intersect
    ( List.filter
        ( \e ->
            not (allowedImpliedByExtension dflags e)
              && not (allowedLanguageEditionExtension dflags e)
        )
        $ incompatibleExtensions minVersion
    )
    . fmap removeSwitch
    $ Plugins.extensions dflags
  where
    removeSwitch onOff = case onOff of
      Plugins.Off a -> a
      Plugins.On a -> a
#else
usedIncompatibleExtensions minVersion dflags =
  List.filter
    ( \e ->
        e `Plugins.xopt` dflags
          && not (allowedImpliedByExtension dflags e)
          && not (allowedLanguageEditionExtension dflags e)
    )
    $ incompatibleExtensions minVersion
#endif

languageEdition :: Plugins.DynFlags -> Maybe.Maybe Plugins.Language
languageEdition = Maybe.maybe def pure . Plugins.language
#if MIN_VERSION_ghc(9, 2, 1)
  where def = pure Plugins.GHC2021
#else
  where def = Maybe.Nothing
#endif

allowedLanguageEditionExtension ::
  Plugins.DynFlags -> GhcRelease.Extension -> Bool
allowedLanguageEditionExtension dflags ext =
  Maybe.maybe
    Bool.False
    (\lang -> ext `List.elem` languageEditionExtensions lang)
    $ languageEdition dflags

allowedImpliedByExtension :: Plugins.DynFlags -> GhcRelease.Extension -> Bool
allowedImpliedByExtension dflags ext =
  -- NB: In this case we /always/ want `Plugins.xopt`, because the implication
  --     only occurs when the extension is enabled.
  Foldable.any (`Plugins.xopt` dflags) $ impliedByExtensions ext

-- | A list of /all/ extensions that are incompatible with the provided version.
incompatibleExtensions :: Version -> [GhcRelease.Extension]
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

-- | GHC before 8.2 doesn’t define `Eq` for `Plugins.Language`, so this does it.
eqLanguage :: Plugins.Language -> Plugins.Language -> Bool
#if MIN_VERSION_ghc(8, 2, 1)
eqLanguage = (==)
#else
eqLanguage x y = case (x, y) of
  (Plugins.Haskell98, Plugins.Haskell98) -> Bool.True
  (Plugins.Haskell2010, Plugins.Haskell2010) -> Bool.True
  (_, _) -> Bool.False
#endif

-- | A language edition may fool us by setting an extension (even negatively) on
--   a newer GHC that it wouldn’t (need to) set on an older one. This whitelists
--   those extensions.
--
--  __TODO__: It would still be good to give an info message in this case,
--            because we can’t know if the extension was implicit from the
--            language edition or explicit.
--          - Actually, we /can/ know if the `OnOff` is the opposite of the one
--            the edition sets.
languageEditionExtensions :: Plugins.Language -> [GhcRelease.Extension]
languageEditionExtensions language =
  snd <=< List.filter (eqLanguage language . fst) $
    Foldable.foldMap GhcRelease.newEditions GhcRelease.all

-- | Sometimes an extension from a later release may be implied by an extension
--   from an earlier relase. This can fool use, because the implied extension
--   will be included in the enabled extensions, so we need to whitelist those
--   /when/ an extension that implies it is enabled.
--
--  __TODO__: Like with `languageEditionExtensions`, we should be smart enough
--            to recognize that we should still warn about @NoFoo@, even if
--            @Foo@ was implied by a supported extension (ad reverse if @NoFoo@
--            is the implied form).
impliedByExtensions :: GhcRelease.Extension -> [GhcRelease.Extension]
impliedByExtensions = Foldable.foldMap GhcRelease.newImplications GhcRelease.all
