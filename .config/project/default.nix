### All available options for this file are listed in
### https://sellout.github.io/project-manager/options.xhtml
{
  config,
  lib,
  self,
  ...
}: {
  project = {
    name = "ghc-compat-plugin";
    summary = "Eases support for multiple GHC versions";
    ## TODO: Move something like this to Flaky.
    file = let
      copyLicenses = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.commercial".source = ../../LICENSE.commercial;
      };
    in
      copyLicenses "core";
  };

  imports = [./hlint.nix];

  ## Current versions of HLint and Ormolu don't yet support GHC 9.14, so skip
  ## the file that requires it.
  programs.treefmt.settings.formatter = {
    hlint.excludes = ["*/Min_9_14_1.hs"];
    ormolu.excludes = ["*/Min_9_14_1.hs"];
  };

  ## CI
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc:
          ## Don’t add `exclude`d matrix entries to the required list
          ##
          ## TODO: Make this less manual (like the `include` component).
            if
              ## GHC before 8.4 needs an older Ubuntu
              lib.versionOlder ghc "8.4"
              && sys == "ubuntu-24.04"
              ## GHC doesn’t support ARM before GHC 9.2.
              || lib.versionOlder ghc "9.2"
              && builtins.elem sys ["macos-15" "ubuntu-24.04-arm"]
              ## GHC 9.2.1 relied on libnuma at runtime for aarch64
              || ghc == "9.2.1" && sys == "ubuntu-24.04-arm"
              ## Plugins are broken on GHC 8.6.1 on Windows, need to use 8.6.2 instead.
              || ghc == "8.6.1" && sys == "windows-2025"
              ## Test is unreliable – we still run it, but don’t require it (maybe related: https://gitlab.haskell.org/ghc/ghc/-/issues/19222)
              || ghc == "8.10.1" && sys == "windows-2025"
              ## UNKNOWN
              || (ghc == "9.2.1" || ghc == "9.2.4" || ghc == "9.4.1") && sys == "macos-15"
            then []
            else [
              "build (${ghc}, ${sys})"
              "build (--prefer-oldest, ${ghc}, ${sys})"
            ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems
      ## Add `include`d matrix entries to the required list.
      ++ map (
        entry:
          if entry.bounds == ""
          then "build (${entry.ghc}, ${entry.os})"
          else "build (${entry.bounds}, ${entry.ghc}, ${entry.os})"
      )
      config.services.haskell-ci.include);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {"${config.project.name}" = "core";};
    ## We use a Cabal version too old for `^>=` bounds.
    checkBounds.enable = false;
    ## The latest Stackage LTS that we also build on GitHub for.
    latestGhcVersion = "9.10.1";
    exclude =
      [
        ## Plugins are broken on GHC 8.6.1 on Windows, need to use 8.6.2 instead.
        ## https://gitlab.haskell.org/ghc/ghc/-/commit/d2cd150eda599d0de83fc687efc48e22a2e74a5e
        {
          ghc = "8.6.1";
          os = "windows-2025";
        }
      ]
      ## UNKNOWN: TESTS FAIL … but tests are NOP
      ++ map (ghc: {
        inherit ghc;
        os = "macos-15";
      }) ["9.2.1" "9.2.4" "9.4.1"];

    include =
      lib.concatMap (bounds: [
        {
          inherit bounds;
          ghc = "8.6.2";
          os = "windows-2025";
        }
        {
          inherit bounds;
          ghc = "9.4.8";
          os = "macos-15";
        }
      ])
      ["" "--prefer-oldest"];
  };

  ## publishing
  services.github.settings.repository.topics = ["compatibility" "plugin"];
}
