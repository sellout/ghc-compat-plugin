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
-- The extensions and warnings added in each version of GHC.
module GhcCompat.GhcRelease
  ( Extension,
    GhcRelease,
    all,
    newEditions,
    newExtensions,
    newImplications,
    newWarnings,
    version,

    -- * individual releases
    ghc_6_0_1,
    ghc_6_8_1,
    ghc_6_10_1,
    ghc_6_12_1,
    ghc_7_0_1,
    ghc_7_2_1,
    ghc_7_4_1,
    ghc_7_6_1,
    ghc_7_8_1,
    ghc_7_10_1,
    ghc_8_0_1,
    ghc_8_2_1,
    ghc_8_4_1,
    ghc_8_6_1,
    ghc_8_8_1,
    ghc_8_10_1,
    ghc_9_0_1,
    ghc_9_2_1,
    ghc_9_2_4,
    ghc_9_6_1,
    ghc_9_8_1,
    ghc_9_10_1,
    ghc_9_12_1,
    ghc_9_14_1,
  )
where

import safe "base" Control.Applicative (pure)
import safe "base" Data.Int (Int)
import safe "base" Data.Version (Version, makeVersion)
#if MIN_VERSION_ghc(8, 0, 0)
import safe qualified "ghc-boot-th" GHC.LanguageExtensions.Type as Extension
#else
import qualified "ghc" GhcPlugins as Extension
#endif
#if MIN_VERSION_ghc(9, 0, 0)
import qualified "ghc" GHC.Plugins as Plugins
#else
import qualified "ghc" GhcPlugins as Plugins
#endif

#if MIN_VERSION_ghc(8, 0, 0)
type Extension = Extension.Extension
#else
type Extension = Plugins.ExtensionFlag
#endif

-- Redundant brackets are due to CPP conditionalization, and removing them would
-- only make the conditionalization worse.
{-# HLINT ignore "Redundant bracket" #-}

data GhcRelease = GhcRelease
  { version :: Version,
    -- | The same `Plugins.Language` can have an entry in multiple
    --   `GhcRelease`s, it just includes the new `Extension`s that were added in
    --   that release.
    newEditions :: [(Plugins.Language, [Extension])],
    newExtensions :: [Extension],
    newImplications :: Extension -> [Extension],
    newWarnings :: [(GhcRelease, [Plugins.WarningFlag])]
  }

ghcRelease :: [Int] -> GhcRelease
ghcRelease versionComponents =
  GhcRelease
    { version = makeVersion versionComponents,
      newEditions = [],
      newExtensions = [],
      newImplications = pure [],
      newWarnings = []
    }

ghc_6_0_1 :: GhcRelease
ghc_6_0_1 =
  (ghcRelease [6, 0, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newExtensions = [Extension.TemplateHaskell]
    }
#else
    { newExtensions = [Extension.Opt_TemplateHaskell]
    }
#endif

ghc_6_8_1 :: GhcRelease
ghc_6_8_1 =
  (ghcRelease [6, 8, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newEditions =
        [ ( Plugins.Haskell98,
            [ Extension.ImplicitPrelude,
              Extension.MonomorphismRestriction
            ]
          ),
          ( Plugins.Haskell2010,
            [ Extension.EmptyDataDecls,
              Extension.ForeignFunctionInterface,
              Extension.ImplicitPrelude,
              Extension.MonomorphismRestriction,
              Extension.PatternGuards
              -- Extension.RelaxedPolyRec – when does this one show up?
            ]
          )
        ],
      newExtensions =
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
        ]
    }
#else
    { newEditions =
        [ ( Plugins.Haskell98,
            [ Extension.Opt_ImplicitPrelude,
              Extension.Opt_MonomorphismRestriction
            ]
          ),
          ( Plugins.Haskell2010,
            [ Extension.Opt_ImplicitPrelude,
              Extension.Opt_MonomorphismRestriction,
              Extension.Opt_EmptyDataDecls,
              Extension.Opt_ForeignFunctionInterface,
              Extension.Opt_PatternGuards
            ]
          )
        ],
      newExtensions =
        [ Extension.Opt_Arrows,
          Extension.Opt_BangPatterns,
          Extension.Opt_ConstrainedClassMethods,
          Extension.Opt_Cpp,
          Extension.Opt_DeriveDataTypeable,
          Extension.Opt_DisambiguateRecordFields,
          Extension.Opt_EmptyDataDecls,
          Extension.Opt_ExistentialQuantification,
          Extension.Opt_ExtendedDefaultRules,
          Extension.Opt_FlexibleContexts,
          Extension.Opt_FlexibleInstances,
          Extension.Opt_ForeignFunctionInterface,
          Extension.Opt_FunctionalDependencies,
          Extension.Opt_GADTs,
          Extension.Opt_GeneralizedNewtypeDeriving,
          Extension.Opt_ImplicitParams,
          Extension.Opt_ImplicitPrelude,
          -- deprecated in favor of @INCOHERENT@ pragma
          Extension.Opt_IncoherentInstances,
          Extension.Opt_KindSignatures,
          Extension.Opt_LiberalTypeSynonyms,
          Extension.Opt_MagicHash,
          Extension.Opt_MonomorphismRestriction,
          Extension.Opt_MultiParamTypeClasses,
          Extension.Opt_OverlappingInstances,
          -- deprecated in favor of @OVERLAP…@ pragmas
          Extension.Opt_OverloadedStrings,
          Extension.Opt_ParallelListComp,
          Extension.Opt_PatternGuards,
          Extension.Opt_RankNTypes,
          Extension.Opt_RecordWildCards,
          Extension.Opt_RecursiveDo,
          Extension.Opt_ScopedTypeVariables,
          Extension.Opt_StandaloneDeriving,
          Extension.Opt_TypeFamilies,
          Extension.Opt_TypeOperators,
          Extension.Opt_TypeSynonymInstances,
          Extension.Opt_UnboxedTuples,
          Extension.Opt_UndecidableInstances,
          Extension.Opt_UnicodeSyntax,
          Extension.Opt_UnliftedFFITypes
        ]
    }
#endif

ghc_6_10_1 :: GhcRelease
ghc_6_10_1 =
  (ghcRelease [6, 10, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newExtensions =
        [ Extension.PackageImports,
          Extension.QuasiQuotes,
          Extension.TransformListComp,
          Extension.ViewPatterns,
-- NB: `NamedFieldPuns` is documented as being added in 6.10.1, but it was
--     called `RecordPuns` in `Extension` until 9.4.1.
#if MIN_VERSION_GLASGOW_HASKELL(9, 4, 1, 0)
          Extension.NamedFieldPuns
#else
          Extension.RecordPuns
#endif
        ]
    }
#else
    { newExtensions =
        [ Extension.Opt_PackageImports,
          Extension.Opt_QuasiQuotes,
          Extension.Opt_RecordPuns,
          Extension.Opt_TransformListComp,
          Extension.Opt_ViewPatterns
        ]
    }
#endif

ghc_6_12_1 :: GhcRelease
ghc_6_12_1 =
  (ghcRelease [6, 12, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newEditions = [(Plugins.Haskell98, [Extension.NPlusKPatterns])],
      newExtensions =
        [ Extension.ExplicitForAll,
          Extension.GHCForeignImportPrim,
          Extension.MonoLocalBinds,
          Extension.NPlusKPatterns,
          Extension.TupleSections
        ],
      newImplications = \ext -> case ext of
        Extension.ExplicitForAll ->
          [ Extension.ScopedTypeVariables,
            Extension.LiberalTypeSynonyms,
            Extension.RankNTypes,
            Extension.ExistentialQuantification
          ]
        Extension.MonoLocalBinds ->
          [ Extension.TypeFamilies,
            Extension.GADTs
          ]
        _ -> []
    }
#else
    { newEditions = [(Plugins.Haskell98, [Extension.Opt_NPlusKPatterns])],
      newExtensions =
        [ Extension.Opt_ExplicitForAll,
          Extension.Opt_GHCForeignImportPrim,
          Extension.Opt_MonoLocalBinds,
          Extension.Opt_NPlusKPatterns,
          Extension.Opt_TupleSections
        ],
      newImplications = \ext -> case ext of
        Extension.Opt_ExplicitForAll ->
          [ Extension.Opt_ScopedTypeVariables,
            Extension.Opt_LiberalTypeSynonyms,
            Extension.Opt_RankNTypes,
            Extension.Opt_ExistentialQuantification
          ]
        Extension.Opt_MonoLocalBinds ->
          [ Extension.Opt_TypeFamilies,
            Extension.Opt_GADTs
          ]
        _ -> []
    }
#endif

ghc_7_0_1 :: GhcRelease
ghc_7_0_1 =
  (ghcRelease [7, 0, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.DatatypeContexts]),
          ( Plugins.Haskell2010,
            [ Extension.DatatypeContexts,
              Extension.DoAndIfThenElse
            ]
          )
        ],
      newExtensions =
        [ Extension.DatatypeContexts,
          Extension.DoAndIfThenElse,
          Extension.RebindableSyntax
        ]
    }
#else
    { newEditions =
        [ (Plugins.Haskell98, [Extension.Opt_DatatypeContexts]),
          ( Plugins.Haskell2010,
            [ Extension.Opt_DatatypeContexts,
              Extension.Opt_DoAndIfThenElse
            ]
          )
        ],
      newExtensions =
        [ Extension.Opt_DatatypeContexts,
          Extension.Opt_DoAndIfThenElse,
          Extension.Opt_RebindableSyntax
        ]
    }
#endif

ghc_7_2_1 :: GhcRelease
ghc_7_2_1 =
  (ghcRelease [7, 2, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newEditions = [(Plugins.Haskell98, [Extension.NondecreasingIndentation])],
      newExtensions =
        [ Extension.DefaultSignatures,
          Extension.DeriveGeneric,
          Extension.GADTSyntax,
          Extension.InterruptibleFFI,
          Extension.MonadComprehensions,
          Extension.NondecreasingIndentation
        ],
      newImplications = \ext -> case ext of
        Extension.GADTSyntax -> [Extension.GADTs]
        _ -> []
    }
#else
    { newEditions =
        [(Plugins.Haskell98, [Extension.Opt_NondecreasingIndentation])],
      newExtensions =
        [ Extension.Opt_DefaultSignatures,
          Extension.Opt_DeriveGeneric,
          Extension.Opt_GADTSyntax,
          Extension.Opt_InterruptibleFFI,
          Extension.Opt_MonadComprehensions,
          Extension.Opt_NondecreasingIndentation
        ],
      newImplications = \ext -> case ext of
        Extension.Opt_GADTSyntax -> [Extension.Opt_GADTs]
        _ -> []
    }
#endif

ghc_7_4_1 :: GhcRelease
ghc_7_4_1 =
  (ghcRelease [7, 4, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.TraditionalRecordSyntax]),
          (Plugins.Haskell2010, [Extension.TraditionalRecordSyntax])
        ],
      newExtensions =
        [ Extension.ConstraintKinds,
          Extension.DataKinds,
          Extension.PolyKinds,
          Extension.TraditionalRecordSyntax
        ]
    }
#else
    { newEditions =
        [ (Plugins.Haskell98, [Extension.Opt_TraditionalRecordSyntax]),
          (Plugins.Haskell2010, [Extension.Opt_TraditionalRecordSyntax])
        ],
      newExtensions =
        [ Extension.Opt_ConstraintKinds,
          Extension.Opt_DataKinds,
          Extension.Opt_PolyKinds,
          Extension.Opt_TraditionalRecordSyntax
        ]
    }
#endif

ghc_7_6_1 :: GhcRelease
ghc_7_6_1 =
  (ghcRelease [7, 6, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
    { newExtensions =
        [ Extension.CApiFFI,
          Extension.ExplicitNamespaces,
          Extension.InstanceSigs,
          Extension.LambdaCase,
          Extension.MultiWayIf
        ],
      newImplications = \ext -> case ext of
        Extension.ExplicitNamespaces ->
          [ Extension.TypeOperators,
            Extension.TypeFamilies
          ]
        _ -> []
    }
#else
    { newExtensions =
        [ Extension.Opt_CApiFFI,
          Extension.Opt_ExplicitNamespaces,
          Extension.Opt_InstanceSigs,
          Extension.Opt_LambdaCase,
          Extension.Opt_MultiWayIf
        ],
      newImplications = \ext -> case ext of
        Extension.Opt_ExplicitNamespaces ->
          [ Extension.Opt_TypeOperators,
            Extension.Opt_TypeFamilies
          ]
        _ -> []
    }
#endif

ghc_7_8_1 :: GhcRelease
ghc_7_8_1 =
  (ghcRelease [7, 8, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
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
        ]
    }
#else
    { newExtensions =
        [ Extension.Opt_AllowAmbiguousTypes,
          Extension.Opt_EmptyCase,
          Extension.Opt_NegativeLiterals,
          -- deprecated in favor of `MultiParamTypeClasses`
          Extension.Opt_NullaryTypeClasses,
          Extension.Opt_NumDecimals,
          Extension.Opt_OverloadedLists,
          Extension.Opt_PatternSynonyms,
          Extension.Opt_RoleAnnotations
        ]
    }
#endif

ghc_7_10_1 :: GhcRelease
ghc_7_10_1 =
  (ghcRelease [7, 10, 1])
#if MIN_VERSION_GLASGOW_HASKELL(8, 0, 1, 0)
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
      -- TODO: In GHC 7.8, @AutoDeriveTypeable@ can be used, but maybe need an
      --       option to decide whether that’s good enough. Especially because
      --       @AutoDeriveTypeable@ is removed in newer GHC versions (so it
      --       would need CPP conditionalization).
      newWarnings = [(ghc_7_8_1, [Plugins.Opt_WarnDerivingTypeable])]
    }
#elif MIN_VERSION_GLASGOW_HASKELL(7, 10, 1, 0)
    { newExtensions =
        [ Extension.Opt_BinaryLiterals,
          Extension.Opt_DeriveAnyClass,
          Extension.Opt_DeriveFoldable,
          Extension.Opt_DeriveFunctor,
          Extension.Opt_DeriveTraversable,
          Extension.Opt_NamedWildCards,
          Extension.Opt_PartialTypeSignatures,
          Extension.Opt_PostfixOperators,
          Extension.Opt_StaticPointers
        ],
      -- TODO: In GHC 7.8, @AutoDeriveTypeable@ can be used, but maybe need an
      --       option to decide whether that’s good enough. Especially because
      --       @AutoDeriveTypeable@ is removed in newer GHC versions (so it
      --       would need CPP conditionalization).
      newWarnings = [(ghc_7_8_1, [Plugins.Opt_WarnDerivingTypeable])]
    }
#endif

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
      newImplications = \ext -> case ext of
        Extension.TemplateHaskellQuotes -> [Extension.TemplateHaskell]
        _ -> []
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
      newImplications = \ext -> case ext of
        Extension.UnboxedSums -> [Extension.UnboxedTuples]
        _ -> []
    }
#endif

ghc_8_4_1 :: GhcRelease
ghc_8_4_1 =
  (ghcRelease [8, 4, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(8, 4, 0, 0)
    { newExtensions =
        [ Extension.EmptyDataDeriving,
          Extension.HexFloatLiterals
        ]
    }
#endif

ghc_8_6_1 :: GhcRelease
ghc_8_6_1 =
  (ghcRelease [8, 6, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(8, 6, 0, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.StarIsType]),
          (Plugins.Haskell2010, [Extension.StarIsType])
        ],
      newExtensions =
        [ Extension.BlockArguments,
          Extension.DerivingVia,
          Extension.NumericUnderscores,
          Extension.QuantifiedConstraints,
          Extension.StarIsType
        ],
      newWarnings = [(ghc_8_6_1, [Plugins.Opt_WarnStarIsType])]
    }
#endif

ghc_8_8_1 :: GhcRelease
ghc_8_8_1 =
  (ghcRelease [8, 8, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(8, 8, 0, 0)
    { newWarnings = [(ghc_8_2_1, [Plugins.Opt_WarnMissingDerivingStrategies])]
    }
#endif

ghc_8_10_1 :: GhcRelease
ghc_8_10_1 =
  (ghcRelease [8, 10, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(8, 10, 0, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.CUSKs]),
          (Plugins.Haskell2010, [Extension.CUSKs])
        ],
      newExtensions =
        [ Extension.CUSKs,
          Extension.ImportQualifiedPost,
          Extension.StandaloneKindSignatures,
          Extension.UnliftedNewtypes
        ],
      newWarnings = [(ghc_8_10_1, [Plugins.Opt_WarnPrepositiveQualifiedModule])]
    }
#endif

ghc_9_0_1 :: GhcRelease
ghc_9_0_1 =
  (ghcRelease [9, 0, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
    { newExtensions =
        [ Extension.LexicalNegation,
          Extension.LinearTypes,
          Extension.QualifiedDo
        ]
    }
#endif

ghc_9_2_1 :: GhcRelease
ghc_9_2_1 =
  (ghcRelease [9, 2, 1])

-- Some versions of GHC error if the third field isn’t 0.
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 0, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.FieldSelectors]),
          (Plugins.Haskell2010, [Extension.FieldSelectors]),
          ( Plugins.GHC2021,
            [ Extension.BangPatterns,
              Extension.BinaryLiterals,
              Extension.ConstrainedClassMethods,
              Extension.ConstraintKinds,
              Extension.DeriveDataTypeable,
              Extension.DeriveFoldable,
              Extension.DeriveFunctor,
              Extension.DeriveGeneric,
              Extension.DeriveLift,
              Extension.DeriveTraversable,
              Extension.DoAndIfThenElse,
              Extension.EmptyCase,
              Extension.EmptyDataDecls,
              Extension.EmptyDataDeriving,
              Extension.ExistentialQuantification,
              Extension.ExplicitForAll,
              Extension.ExplicitNamespaces, -- negative
              Extension.FieldSelectors,
              Extension.FlexibleContexts,
              Extension.FlexibleInstances,
              Extension.ForeignFunctionInterface,
              Extension.GADTSyntax,
              Extension.GeneralizedNewtypeDeriving,
              Extension.HexFloatLiterals,
              Extension.ImplicitPrelude,
              Extension.ImportQualifiedPost,
              Extension.InstanceSigs,
              Extension.KindSignatures,
              Extension.MonomorphismRestriction,
              Extension.MultiParamTypeClasses,
#if MIN_VERSION_GLASGOW_HASKELL(9, 4, 1, 0)
              Extension.NamedFieldPuns,
#else
              Extension.RecordPuns
#endif
              Extension.NamedWildCards,
              Extension.NumericUnderscores,
              Extension.PatternGuards,
              Extension.PolyKinds,
              Extension.PostfixOperators,
              Extension.RankNTypes,
              Extension.RelaxedPolyRec,
              Extension.ScopedTypeVariables,
              Extension.StandaloneDeriving,
              Extension.StandaloneKindSignatures,
              Extension.StarIsType,
              Extension.TraditionalRecordSyntax,
              Extension.TupleSections,
              Extension.TypeApplications,
              Extension.TypeOperators,
              Extension.TypeSynonymInstances
            ]
          )
        ],
      newExtensions =
        [ Extension.FieldSelectors,
          -- added in GHC 6.10, but unreliable before 9.2
          Extension.ImpredicativeTypes,
          Extension.OverloadedRecordDot,
          Extension.OverloadedRecordUpdate,
          Extension.UnliftedDatatypes
        ],
      newWarnings = [(ghc_6_8_1, [Plugins.Opt_WarnMissingKindSignatures])]
    }
#endif

ghc_9_2_4 :: GhcRelease
ghc_9_2_4 =
  (ghcRelease [9, 2, 4])
#if MIN_VERSION_GLASGOW_HASKELL(9, 2, 4, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.DeepSubsumption]),
          (Plugins.Haskell2010, [Extension.DeepSubsumption])
        ],
      newExtensions = [Extension.DeepSubsumption]
    }
#endif

ghc_9_6_1 :: GhcRelease
ghc_9_6_1 =
  (ghcRelease [9, 6, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 1, 0)
    { newExtensions = [Extension.TypeData]
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
      newWarnings =
        [ (ghc_7_4_1, [Plugins.Opt_WarnMissingPolyKindSignatures]),
          (ghc_7_8_1, [Plugins.Opt_WarnMissingRoleAnnotations])
        ]
    }
#endif

ghc_9_10_1 :: GhcRelease
ghc_9_10_1 =
  (ghcRelease [9, 10, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 1, 0)
    { newEditions =
        [ ( Plugins.GHC2024,
            [ Extension.BangPatterns,
              Extension.BinaryLiterals,
              Extension.ConstrainedClassMethods,
              Extension.ConstraintKinds,
              Extension.DataKinds,
              Extension.DeriveDataTypeable,
              Extension.DeriveFoldable,
              Extension.DeriveFunctor,
              Extension.DeriveGeneric,
              Extension.DeriveLift,
              Extension.DeriveTraversable,
              Extension.DerivingStrategies,
              Extension.DisambiguateRecordFields,
              Extension.DoAndIfThenElse,
              Extension.EmptyCase,
              Extension.EmptyDataDecls,
              Extension.EmptyDataDeriving,
              Extension.ExistentialQuantification,
              Extension.ExplicitForAll,
              Extension.ExplicitNamespaces, -- positive
              Extension.FieldSelectors,
              Extension.FlexibleContexts,
              Extension.FlexibleInstances,
              Extension.ForeignFunctionInterface,
              Extension.GADTSyntax,
              Extension.GADTs,
              Extension.GeneralizedNewtypeDeriving,
              Extension.HexFloatLiterals,
              Extension.ImplicitPrelude,
              Extension.ImportQualifiedPost,
              Extension.InstanceSigs,
              Extension.KindSignatures,
              Extension.LambdaCase,
              Extension.MonoLocalBinds,
              Extension.MonomorphismRestriction,
              Extension.MultiParamTypeClasses,
              Extension.NamedFieldPuns,
              Extension.NamedWildCards,
              Extension.NumericUnderscores,
              Extension.PatternGuards,
              Extension.PolyKinds,
              Extension.PostfixOperators,
              Extension.RankNTypes,
              Extension.RelaxedPolyRec,
              Extension.RoleAnnotations,
              Extension.ScopedTypeVariables,
              Extension.StandaloneDeriving,
              Extension.StandaloneKindSignatures,
              Extension.StarIsType,
              Extension.TraditionalRecordSyntax,
              Extension.TupleSections,
              Extension.TypeApplications,
              Extension.TypeOperators,
              Extension.TypeSynonymInstances
            ]
          )
        ],
      newExtensions =
        [ Extension.ListTuplePuns,
          Extension.RequiredTypeArguments
        ]
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
        ]
    }
#endif

ghc_9_14_1 :: GhcRelease
ghc_9_14_1 =
  (ghcRelease [9, 14, 1])
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 0)
    { newEditions =
        [ (Plugins.Haskell98, [Extension.ImplicitStagePersistence]),
          (Plugins.Haskell2010, [Extension.ImplicitStagePersistence]),
          (Plugins.GHC2021, [Extension.ImplicitStagePersistence]),
          (Plugins.GHC2024, [Extension.ImplicitStagePersistence])
        ],
      newExtensions =
        [ Extension.ExplicitLevelImports,
          Extension.ImplicitStagePersistence
        ],
      newWarnings = [(ghc_9_14_1, [Plugins.Opt_WarnPatternNamespaceSpecifier])]
    }
#endif

all :: [GhcRelease]
all =
  [ ghc_6_0_1,
    ghc_6_8_1,
    ghc_6_10_1,
    ghc_6_12_1,
    ghc_7_0_1,
    ghc_7_2_1,
    ghc_7_4_1,
    ghc_7_6_1,
    ghc_7_8_1,
    ghc_7_10_1,
    ghc_8_0_1,
    ghc_8_2_1,
    ghc_8_4_1,
    ghc_8_6_1,
    ghc_8_8_1,
    ghc_8_10_1,
    ghc_9_0_1,
    ghc_9_2_1,
    ghc_9_2_4,
    ghc_9_6_1,
    ghc_9_8_1,
    ghc_9_10_1,
    ghc_9_12_1,
    ghc_9_14_1
  ]
