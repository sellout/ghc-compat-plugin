{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Ensures newer compilers don’t complain about features that aren’t available in older compilers you support.
--
--   For example, if you support GHC versions 9.0 through 9.14, you could pass GHC options like
--
--   @-Weverything -fplugin GhcCompat -fplugin-opt GhcCompat:minVersion=9.2@
--
--   and it would prevent warnings about missing kind signatures, missing
--   poly-kind signatures, and use of the @pattern@ namespace qualifier because
--   addressing those warnings requires features not available in GHC 9.0.
--
--  __TODO__:
--
--          - @-Weverything@ was added in GHC 8.0 – add an option that will set
--            @-Weverything@ then, or manually enable all warnings on older
--            GHCs.
--
--          - Warn if extensions that were added after the min version are being
--            used.
--
--          - Add opts to control each extensions and option. This is because
--            they may be used only in code that is already conditionalized on
--            the compiler version, and so are safe to ignore.
--
--          - Warn if the current compiler is older than the min version
--            provided.
--
--          - Make this a NOP plugin when the current GHC is older than we
--            support. This allows users to use it across older versions without
--            them having to conditionally depend on the plugin. The way to do
--            this is to move the code to a different module and conditionalize
--            the import, rather than wrapping all the code in CPP.
module GhcCompat.Supported
  ( plugin,

    -- * exported purely for documentation
    Opts,
    defaultOpts,

    -- * exported to avoid extra CPP
    ghc_6_8_1,
    ghc_7_4_1,
    ghc_7_8_1,
    ghc_8_2_1,
    ghc_8_6_1,
    ghc_8_10_1,
    ghc_9_14_1,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Eq (Eq, (/=))
import safe "base" Data.Foldable (concatMap, foldl')
import safe "base" Data.Function (const, flip, ($))
import safe "base" Data.List (drop, dropWhile, head, take)
import safe "base" Data.Monoid (mconcat)
import safe "base" Data.Ord (Ord, (<))
import safe "base" Data.Tuple (fst, uncurry)
import safe "base" Data.Version (Version, makeVersion, parseVersion)
import safe "base" Text.ParserCombinators.ReadP (readP_to_S)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
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

-- |
--
-- >>> defaultOpts $ makeVersion [7, 10, 1]
-- Opts {minSupportedVersion = Version {versionBranch = [7,10,1], versionTags = []}}
defaultOpts :: Version -> Opts
defaultOpts minSupportedVersion = Opts {minSupportedVersion}

newtype Opts = Opts {minSupportedVersion :: Version}
  deriving (Eq, Ord, Read, Show)

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = \opts -> pure . updateFlags (parseOpts opts),
      pluginRecompile = Plugins.flagRecompile
    }

parseOpts :: [Plugins.CommandLineOption] -> Opts
parseOpts =
  defaultOpts
    . fst
    . head
    . readP_to_S parseVersion
    . (drop 1 . dropWhile (/= '=') <=< take 1)

updateFlags :: Opts -> Plugins.HscEnv -> Plugins.HscEnv
updateFlags Opts {minSupportedVersion} env =
  env
    { Plugins.hsc_dflags =
        foldl' Plugins.wopt_unset (Plugins.extractDynFlags env) $
          warningFlags minSupportedVersion
    }

warningFlags :: Version -> [Plugins.WarningFlag]
warningFlags =
  mconcat
    [ ghc_7_10_flags,
      ghc_8_6_flags,
      ghc_8_8_1_flags,
      ghc_8_10_flags,
      ghc_9_2_flags,
      ghc_9_8_flags,
      ghc_9_8_1_flags,
      ghc_9_14_1_flags
    ]

whenOlder :: Version -> Version -> [Plugins.WarningFlag] -> [Plugins.WarningFlag]
whenOlder minSupportedVersion flagAddedVersion flags =
  if minSupportedVersion < flagAddedVersion then flags else []

ghc_6_8_1 :: Version
ghc_6_8_1 = makeVersion [6, 8, 1]

ghc_7_4_1 :: Version
ghc_7_4_1 = makeVersion [7, 4, 1]

ghc_7_8_1 :: Version
ghc_7_8_1 = makeVersion [7, 8, 1]

ghc_8_2_1 :: Version
ghc_8_2_1 = makeVersion [8, 2, 1]

ghc_8_6_1 :: Version
ghc_8_6_1 = makeVersion [8, 6, 1]

ghc_8_10_1 :: Version
ghc_8_10_1 = makeVersion [8, 10, 1]

ghc_9_14_1 :: Version
ghc_9_14_1 = makeVersion [9, 14, 1]

disableIfOlder ::
  [(Version, [Plugins.WarningFlag])] -> Version -> [Plugins.WarningFlag]
disableIfOlder = flip $ concatMap . uncurry . whenOlder

ghc_7_10_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(7, 10, 0, 0)
ghc_7_10_flags =
  -- TODO: In GHC 7.8, @AutoDeriveTypeable@ can be used, but maybe need an
  --       option to decide whether that’s good enough. Especially because
  --       @AutoDeriveTypeable@ is removed in newer GHC versions (so it would
  --       need CPP conditionalization).
  disableIfOlder [(ghc_7_8_1, [Plugins.Opt_WarnDerivingTypeable])]
#else
ghc_7_10_flags = const []
#endif

ghc_8_6_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0)
ghc_8_6_flags = disableIfOlder [(ghc_8_6_1, [Plugins.Opt_WarnStarIsType])]
#else
ghc_8_6_flags = const []
#endif

ghc_8_8_1_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0)
ghc_8_8_1_flags =
  disableIfOlder [(ghc_8_2_1, [Plugins.Opt_WarnMissingDerivingStrategies])]
#else
ghc_8_8_1_flags = const []
#endif

ghc_8_10_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
ghc_8_10_flags =
  disableIfOlder [(ghc_8_10_1, [Plugins.Opt_WarnPrepositiveQualifiedModule])]
#else
ghc_8_10_flags = const []
#endif

ghc_9_2_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
ghc_9_2_flags =
  disableIfOlder [(ghc_6_8_1, [Plugins.Opt_WarnMissingKindSignatures])]
#else
ghc_9_2_flags = const []
#endif

ghc_9_8_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 0, 0)
ghc_9_8_flags =
  disableIfOlder [(ghc_7_4_1, [Plugins.Opt_WarnMissingPolyKindSignatures])]
#else
ghc_9_8_flags = const []
#endif

ghc_9_8_1_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 0, 0)
ghc_9_8_1_flags =
  disableIfOlder [(ghc_7_8_1 ,[Plugins.Opt_WarnMissingRoleAnnotations])]
#else
ghc_9_8_1_flags = const []
#endif

ghc_9_14_1_flags :: Version -> [Plugins.WarningFlag]
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
ghc_9_14_1_flags =
  disableIfOlder [(ghc_9_14_1, [Plugins.Opt_WarnPatternNamespaceSpecifier])]
#else
ghc_9_14_1_flags = const []
#endif
