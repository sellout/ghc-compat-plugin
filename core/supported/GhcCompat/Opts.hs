{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Option handling for "GhcCompat".
module GhcCompat.Opts
  ( Opts (Opts),
    ReportLevel (Error, Warn),
    minVersion,
    parse,
    reportIncompatibleExtensions,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (second)
import "base" Data.Either (Either (Left))
import "base" Data.Eq ((==))
import "base" Data.Foldable (foldrM)
import "base" Data.Function (flip, ($))
import "base" Data.Functor (fmap, (<$>))
import "base" Data.List (break, drop, lookup, reverse, uncons)
import "base" Data.Maybe (Maybe (Nothing), maybe)
import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" Data.Tuple (fst, uncurry)
import "base" Data.Version (Version, parseVersion)
import "base" Text.ParserCombinators.ReadP (readP_to_S)

-- | This mirrors the levels provided by GHC’s warning flags. Correspondingly,
--   we use the lowercase forms for the plugin opts instead of the capitalized
--   ones.
data ReportLevel = Warn | Error

defaultOpts :: Version -> Opts
defaultOpts minVersion =
  Opts {minVersion, reportIncompatibleExtensions = pure Warn}

readVersion :: String -> Maybe Version
readVersion = fmap (fst . fst) . uncons . reverse . readP_to_S parseVersion

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

parseVersion' :: String -> Either String Version
parseVersion' versionStr =
  maybe
    (Left $ "Couldn’t parse ‘minVersion’ value ‘" <> versionStr <> "’.")
    pure
    $ readVersion versionStr

parseOpt :: Opts -> String -> String -> Either String Opts
parseOpt opts name value = case (name, value) of
  ("minVersion", version) ->
    (\v -> opts {minVersion = v}) <$> parseVersion' version
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

parse :: [String] -> Either String Opts
parse optStrs =
  let kv = second (drop 1) . break (== '=') <$> optStrs
   in maybe
        (Left "Missing required ‘minVersion’ plugin-opt.")
        ( ( \version ->
              foldrM (flip $ uncurry . parseOpt) (defaultOpts version) $
                reverse kv
          )
            <=< parseVersion'
        )
        $ lookup "minVersion" kv
