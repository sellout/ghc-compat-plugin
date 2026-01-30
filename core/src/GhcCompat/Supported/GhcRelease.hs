{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- | The implementation of the plugin, but this module is only loaded on
--   GHC 7.10+.
module GhcCompat.Supported.GhcRelease
  ( GhcRelease,
    all,
    newExtensions,
    newWarnings,
    version,
  )
where

import safe "base" Data.Int (Int)
import safe "base" Data.Version (Version, makeVersion)
import safe "ghc-boot-th" GHC.LanguageExtensions.Type (Extension)
import safe qualified "ghc-boot-th" GHC.LanguageExtensions.Type as Extension
#if MIN_VERSION_ghc(9, 0, 0)
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif

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

all :: [GhcRelease]
all =
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
