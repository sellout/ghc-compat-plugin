{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

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
module GhcCompat
  ( module Implementation,
  )
where

#if defined (MIN_VERSION_GLASGOW_HASKELL)
import "this" GhcCompat.Supported as Implementation (plugin, Opts, defaultOpts)
#else
import "this" GhcCompat.Unsupported as Implementation (plugin)
#endif
