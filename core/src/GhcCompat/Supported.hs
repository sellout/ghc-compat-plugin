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
import safe "base" Data.Bifunctor (first, second)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (concatMap, foldl', foldrM)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Int (Int)
import safe "base" Data.List (break, drop, intercalate, intersect, lookup)
import safe "base" Data.List.NonEmpty (last, nonEmpty)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.Monoid (mconcat)
import safe "base" Data.Ord (Ord, (<))
import safe "base" Data.Semigroup (sconcat, (<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (curry, fst, uncurry)
import safe "base" Data.Version
  ( Version,
    makeVersion,
    parseVersion,
    showVersion,
  )
import safe "base" System.Exit (die)
import safe "base" System.IO (IO, putStr)
import safe "base" Text.ParserCombinators.ReadP (readP_to_S)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show, show)
import safe "ghc-boot-th" GHC.LanguageExtensions.Type (Extension)
import safe qualified "ghc-boot-th" GHC.LanguageExtensions.Type as Extension
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

-- | This mirrors the levels provided by GHC’s warning flags. Correspondingly,
--   we use the lowercase forms for the plugin opts instead of the capitalized
--   ones.
data ReportLevel = Warn | Error
  deriving (Eq, Ord, Read, Show)

defaultOpts :: Version -> Opts
defaultOpts minVersion =
  Opts {minVersion, reportIncompatibleExtensions = pure Warn}

readVersion :: String -> Maybe Version
readVersion = fmap (fst . last) . nonEmpty . readP_to_S parseVersion

-- | Options support by the plugin. These can be specified with the
--   [@-fplugin-opt@](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#ghc-flag-fplugin-opt-module-args)
--   GHC option.
--
-- >>> defaultOpts <$> readVersion "7.10.1"
-- Just (Opts {minVersion = Version {versionBranch = [7,10,1], versionTags = []}, reportIncompatibleExtensions = Just Warn})
data Opts = Opts
  { -- | Period-separated natural numbers (e.g., “7.10.1”).
    minVersion :: Version,
    -- | This can be “no”, “warn” (the default), or “error”.
    reportIncompatibleExtensions :: Maybe ReportLevel
  }
  deriving (Eq, Ord, Read, Show)

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = \optStrs env ->
        fmap (\dflags -> env {Plugins.hsc_dflags = dflags})
          . dflagsPlugin optStrs
          $ Plugins.extractDynFlags env,
      pluginRecompile = Plugins.flagRecompile
    }

parseOpt :: Opts -> String -> String -> Either String Opts
parseOpt opts =
  curry $ \case
    ("minVersion", _) -> pure opts
    ("reportIncompatibleExtensions", level) ->
      (\v -> opts {reportIncompatibleExtensions = v}) <$> case level of
        "no" -> pure Nothing
        "warn" -> pure $ pure Warn
        "error" -> pure $ pure Error
        _ ->
          Left $
            "Unknown reporting level ‘"
              <> level
              <> "’ (options are ‘no’, ‘warn’, and ‘error’)."
    (k, v) ->
      Left $
        "Received unknown plugin-opt ‘" <> k <> "’ with value ‘" <> v <> "’."

parseOpts :: [Plugins.CommandLineOption] -> Either String Opts
parseOpts optStrs =
  let kv = second (drop 1) . break (== '=') <$> optStrs
   in maybe
        (Left "Missing required ‘minVersion’ plugin-opt.")
        ( \versionStr ->
            maybe
              ( Left $
                  "Couldn’t parse ‘minVersion’ value ‘" <> versionStr <> "’."
              )
              ( \version ->
                  foldrM (flip $ uncurry . parseOpt) (defaultOpts version) kv
              )
              $ readVersion versionStr
        )
        $ lookup "minVersion" kv

errorPrelude :: [Plugins.CommandLineOption] -> String -> String
errorPrelude optStrs prefix =
  "on the commandline: "
    <> prefix
    <> ": [GhcCompat plugin] ["
    <> intercalate ", " optStrs
    <> "]\n    "

-- | A list of extensions incompatible with the provided version that are used
--   (regardless of `Extension.OnOff`).
usedIncompatibleExtensions :: Version -> Plugins.DynFlags -> [Extension]
usedIncompatibleExtensions minVersion =
  intersect (incompatibleExtensions minVersion)
    . fmap removeSwitch
    . Plugins.extensions
  where
    removeSwitch = \case
      Plugins.Off a -> a
      Plugins.On a -> a

dflagsPlugin ::
  [Plugins.CommandLineOption] -> Plugins.DynFlags -> IO Plugins.DynFlags
dflagsPlugin optStrs dflags = do
  opts <-
    either (die . (errorPrelude optStrs "error" <>)) pure $ parseOpts optStrs
  let minVer = minVersion opts
  maybe
    (pure ())
    ( \level ->
        maybe
          (pure ())
          ( ( case level of
                Warn -> putStr . (errorPrelude optStrs "warning" <>)
                Error -> die . (errorPrelude optStrs "error" <>)
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
    $ reportIncompatibleExtensions opts
  pure $ updateFlags minVer dflags

updateFlags :: Version -> Plugins.DynFlags -> Plugins.DynFlags
updateFlags minVersion dflags =
  foldl' Plugins.wopt_unset dflags $ warningFlags minVersion

data GhcRelease = GhcRelease
  { version :: Version,
    newExtensions :: [Extension],
    newWarnings :: [(GhcRelease, [Plugins.WarningFlag])]
  }

ghcRelease :: [Int] -> GhcRelease
ghcRelease versionComponents =
  GhcRelease
    { version = makeVersion versionComponents,
      newExtensions = [],
      newWarnings = []
    }

ghc_6_0 :: GhcRelease
ghc_6_0 =
  (ghcRelease [6, 0])
    { newExtensions = [Extension.TemplateHaskell],
      newWarnings = []
    }

ghc_6_8_1 :: GhcRelease
ghc_6_8_1 =
  (ghcRelease [6, 8, 1])
    { newExtensions =
        [ Extension.Arrows,
          Extension.BangPatterns,
          Extension.ConstrainedClassMethods,
          Extension.Cpp,
          Extension.DeriveDataTypeable,
          Extension.DisambiguateRecordFields,
          Extension.EmptyDataDecls,
          Extension.ExistentialQuantification,
          Extension.ExtendedDefaultRules,
          Extension.FlexibleContexts,
          Extension.FlexibleInstances,
          Extension.ForeignFunctionInterface,
          Extension.FunctionalDependencies,
          Extension.GADTs,
          Extension.GeneralizedNewtypeDeriving,
          Extension.ImplicitParams,
          Extension.ImplicitPrelude,
          -- deprecated in favor of @INCOHERENT@ pragma
          Extension.IncoherentInstances,
          Extension.KindSignatures,
          Extension.LiberalTypeSynonyms,
          Extension.MagicHash,
          Extension.MonomorphismRestriction,
          Extension.MultiParamTypeClasses,
          Extension.OverlappingInstances,
          -- deprecated in favor of @OVERLAP…@ pragmas
          Extension.OverloadedStrings,
          Extension.ParallelListComp,
          Extension.PatternGuards,
          Extension.RankNTypes,
          Extension.RecordWildCards,
          Extension.RecursiveDo,
          Extension.ScopedTypeVariables,
          Extension.StandaloneDeriving,
          Extension.TypeFamilies,
          Extension.TypeOperators,
          Extension.TypeSynonymInstances,
          Extension.UnboxedTuples,
          Extension.UndecidableInstances,
          Extension.UnicodeSyntax,
          Extension.UnliftedFFITypes
        ],
      newWarnings = []
    }

ghc_6_10_1 :: GhcRelease
ghc_6_10_1 =
  (ghcRelease [6, 10, 1])
    { newExtensions =
        [ Extension.NamedFieldPuns,
          Extension.PackageImports,
          Extension.QuasiQuotes,
          Extension.TransformListComp,
          Extension.ViewPatterns
        ],
      newWarnings = []
    }

ghc_6_12 :: GhcRelease
ghc_6_12 =
  (ghcRelease [6, 12])
    { newExtensions = [Extension.TupleSections],
      newWarnings = []
    }

ghc_6_12_1 :: GhcRelease
ghc_6_12_1 =
  (ghcRelease [6, 12, 1])
    { newExtensions =
        [ Extension.ExplicitForAll,
          Extension.GHCForeignImportPrim,
          Extension.MonoLocalBinds,
          Extension.NPlusKPatterns
        ],
      newWarnings = []
    }

ghc_7_0_1 :: GhcRelease
ghc_7_0_1 =
  (ghcRelease [7, 0, 1])
    { newExtensions =
        [ Extension.DatatypeContexts,
          Extension.DoAndIfThenElse,
          Extension.RebindableSyntax
        ],
      newWarnings = []
    }

ghc_7_2_1 :: GhcRelease
ghc_7_2_1 =
  (ghcRelease [7, 2, 1])
    { newExtensions =
        [ Extension.DefaultSignatures,
          Extension.DeriveGeneric,
          Extension.GADTSyntax,
          Extension.InterruptibleFFI,
          Extension.MonadComprehensions,
          Extension.NondecreasingIndentation
        ],
      newWarnings = []
    }

ghc_7_4_1 :: GhcRelease
ghc_7_4_1 =
  (ghcRelease [7, 4, 1])
    { newExtensions =
        [ Extension.ConstraintKinds,
          Extension.DataKinds,
          Extension.PolyKinds,
          Extension.TraditionalRecordSyntax
        ],
      newWarnings = []
    }

ghc_7_6_1 :: GhcRelease
ghc_7_6_1 =
  (ghcRelease [7, 6, 1])
    { newExtensions =
        [ Extension.CApiFFI,
          Extension.ExplicitNamespaces,
          Extension.InstanceSigs,
          Extension.LambdaCase,
          Extension.MultiWayIf
        ],
      newWarnings = []
    }

ghc_7_8_1 :: GhcRelease
ghc_7_8_1 =
  (ghcRelease [7, 8, 1])
    { newExtensions =
        [ Extension.AllowAmbiguousTypes,
          Extension.EmptyCase,
          Extension.NegativeLiterals,
          -- deprecated in favor of `MultiParamTypeClasses`
          Extension.NullaryTypeClasses,
          Extension.NumDecimals,
          Extension.OverloadedLists,
          Extension.PatternSynonyms,
          Extension.RoleAnnotations
        ],
      newWarnings = []
    }

ghc_7_10 :: GhcRelease
ghc_7_10 =
  (ghcRelease [7, 10])
    { newExtensions = [],
      -- TODO: In GHC 7.8, @AutoDeriveTypeable@ can be used, but maybe need an
      --       option to decide whether that’s good enough. Especially because
      --       @AutoDeriveTypeable@ is removed in newer GHC versions (so it
      --       would need CPP conditionalization).
      newWarnings = [(ghc_7_8_1, [Plugins.Opt_WarnDerivingTypeable])]
    }

ghc_7_10_1 :: GhcRelease
ghc_7_10_1 =
  (ghcRelease [7, 10, 1])
    { newExtensions =
        [ Extension.BinaryLiterals,
          Extension.DeriveAnyClass,
          Extension.DeriveFoldable,
          Extension.DeriveFunctor,
          Extension.DeriveTraversable,
          Extension.NamedWildCards,
          Extension.PartialTypeSignatures,
          Extension.PostfixOperators,
          Extension.StaticPointers
        ],
      newWarnings = []
    }

ghc_8_0_1 :: GhcRelease
ghc_8_0_1 =
  (ghcRelease [8, 0, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newExtensions =
        [ Extension.ApplicativeDo,
          Extension.DeriveLift,
          Extension.DuplicateRecordFields,
          Extension.OverloadedLabels,
          Extension.Strict,
          Extension.StrictData,
          Extension.TemplateHaskellQuotes,
          Extension.TypeApplications,
          Extension.TypeFamilyDependencies,
          -- deprecated in favor of `DataKinds` + `PolyKinds`
          Extension.TypeInType,
          Extension.UndecidableSuperClasses
        ],
      newWarnings = []
    }
#endif

ghc_8_2_1 :: GhcRelease
ghc_8_2_1 =
  (ghcRelease [8, 2, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 2, 1, 0)
    { newExtensions =
        [ Extension.DerivingStrategies,
          Extension.UnboxedSums
        ],
      newWarnings = []
    }
#endif

ghc_8_4_1 :: GhcRelease
ghc_8_4_1 =
  (ghcRelease [8, 4, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 4, 1, 0)
    { newExtensions =
        [ Extension.EmptyDataDeriving,
          Extension.HexFloatLiterals
        ],
      newWarnings = []
    }
#endif

ghc_8_6 :: GhcRelease
ghc_8_6 =
  (ghcRelease [8, 6])
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
    { newExtensions = [],
      newWarnings = [(ghc_8_6_1, [Plugins.Opt_WarnStarIsType])]
    }
#endif

ghc_8_6_1 :: GhcRelease
ghc_8_6_1 =
  (ghcRelease [8, 6, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 1, 0)
    { newExtensions =
        [ Extension.BlockArguments,
          Extension.DerivingVia,
          Extension.NumericUnderscores,
          Extension.QuantifiedConstraints,
          Extension.StarIsType
        ],
      newWarnings = []
    }
#endif

ghc_8_8_1 :: GhcRelease
ghc_8_8_1 =
  (ghcRelease [8, 8, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 1, 0)
    { newExtensions = [],
      newWarnings = [(ghc_8_2_1, [Plugins.Opt_WarnMissingDerivingStrategies])]
    }
#endif

ghc_8_10 :: GhcRelease
ghc_8_10 =
  (ghcRelease [8, 10])
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
    { newExtensions = [],
      newWarnings = [(ghc_8_10_1, [Plugins.Opt_WarnPrepositiveQualifiedModule])]
    }
#endif

ghc_8_10_1 :: GhcRelease
ghc_8_10_1 =
  (ghcRelease [8, 10, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 1, 0)
    { newExtensions =
        [ Extension.CUSKs,
          Extension.ImportQualifiedPost,
          Extension.StandaloneKindSignatures,
          Extension.UnliftedNewtypes
        ],
      newWarnings = []
    }
#endif

ghc_9_0_1 :: GhcRelease
ghc_9_0_1 =
  (ghcRelease [9, 0, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 1, 0)
    { newExtensions =
        [ Extension.LexicalNegation,
          Extension.LinearTypes,
          Extension.QualifiedDo
        ],
      newWarnings = []
    }
#endif

ghc_9_2 :: GhcRelease
ghc_9_2 =
  (ghcRelease [9, 2])
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
    { newExtensions =
        [ Extension.OverloadedRecordDot,
          Extension.OverloadedRecordUpdate
        ],
      newWarnings = [(ghc_6_8_1, [Plugins.Opt_WarnMissingKindSignatures])]
    }
#endif

ghc_9_2_1 :: GhcRelease
ghc_9_2_1 =
  (ghcRelease [9, 2, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 1, 0)
    { newExtensions =
        [ Extension.FieldSelectors,
          -- added in GHC 6.10, but unreliable before 9.2
          Extension.ImpredicativeTypes,
          Extension.UnliftedDatatypes
        ],
      newWarnings = []
    }
#endif

ghc_9_2_4 :: GhcRelease
ghc_9_2_4 =
  (ghcRelease [9, 2, 4])
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 4, 0)
    { newExtensions = [Extension.DeepSubsumption],
      newWarnings = []
    }
#endif

ghc_9_6_1 :: GhcRelease
ghc_9_6_1 =
  (ghcRelease [9, 6, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
    { newExtensions = [Extension.TypeData],
      newWarnings = []
    }
#endif

ghc_9_8 :: GhcRelease
ghc_9_8 =
  (ghcRelease [9, 8])
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 0, 0)
    { newExtensions = [],
      newWarnings = [(ghc_7_4_1, [Plugins.Opt_WarnMissingPolyKindSignatures])]
    }
#endif

ghc_9_8_1 :: GhcRelease
ghc_9_8_1 =
  (ghcRelease [9, 8, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
    { newExtensions =
        [ Extension.ExtendedLiterals,
          Extension.TypeAbstractions
        ],
      newWarnings = [(ghc_7_8_1, [Plugins.Opt_WarnMissingRoleAnnotations])]
    }
#endif

ghc_9_10_1 :: GhcRelease
ghc_9_10_1 =
  (ghcRelease [9, 10, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
    {
      newExtensions =
        [ Extension.ListTuplePuns,
          Extension.RequiredTypeArguments
        ],
      newWarnings = []
    }
#endif

ghc_9_12_1 :: GhcRelease
ghc_9_12_1 =
  (ghcRelease [9, 12, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 1, 0)
    { newExtensions =
        [ Extension.MultilineStrings,
          Extension.NamedDefaults,
          Extension.OrPatterns
        ],
      newWarnings = []
    }
#endif

ghc_9_14_1 :: GhcRelease
ghc_9_14_1 =
  (ghcRelease [9, 14, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
    { newExtensions =
        [ Extension.ExplicitLevelImports,
          Extension.ImplicitStagePersistence
        ],
      newWarnings = [(ghc_9_14_1, [Plugins.Opt_WarnPatternNamespaceSpecifier])]
    }
#endif

allGhcs :: [GhcRelease]
allGhcs =
  [ ghc_6_0,
    ghc_6_8_1,
    ghc_6_10_1,
    ghc_6_12,
    ghc_6_12_1,
    ghc_7_0_1,
    ghc_7_2_1,
    ghc_7_4_1,
    ghc_7_6_1,
    ghc_7_8_1,
    ghc_7_10,
    ghc_7_10_1,
    ghc_8_0_1,
    ghc_8_2_1,
    ghc_8_4_1,
    ghc_8_6,
    ghc_8_6_1,
    ghc_8_8_1,
    ghc_8_10,
    ghc_8_10_1,
    ghc_9_0_1,
    ghc_9_2,
    ghc_9_2_1,
    ghc_9_2_4,
    ghc_9_6_1,
    ghc_9_8,
    ghc_9_8_1,
    ghc_9_10_1,
    ghc_9_12_1,
    ghc_9_14_1
  ]

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
  (\ghc -> if minVersion < version ghc then newExtensions ghc else [])
    =<< allGhcs

warningFlags :: Version -> [Plugins.WarningFlag]
warningFlags =
  mconcat $ disableIfOlder . fmap (first version) . newWarnings <$> allGhcs

whenOlder ::
  Version -> Version -> [Plugins.WarningFlag] -> [Plugins.WarningFlag]
whenOlder minVersion flagAddedVersion flags =
  if minVersion < flagAddedVersion then flags else []

disableIfOlder ::
  [(Version, [Plugins.WarningFlag])] -> Version -> [Plugins.WarningFlag]
disableIfOlder = flip $ concatMap . uncurry . whenOlder
