let
  sources = import ./sources.nix;
in
{ compiler ? "ghc883"
, pkgs ? import sources.nixpkgs { }
}:

let
  clean = import ./clean.nix { inherit (pkgs) lib; };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      hakyll = pkgs.haskell.lib.appendConfigureFlags hsuper.hakyll [ "-f" "watchServer" "-f" "previewServer" ];

      site = (hself.callCabal2nix "site" (clean ../.) { }).overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs or [] ++ [
              pkgs.makeWrapper
            ];
            postInstall = old.postInstall or "" + ''
              wrapProgram $out/bin/site \
                --set LANG "en_US.UTF-8" \
                --set LOCALE_ARCHIVE "${pkgs.glibcLocales}/lib/locale/locale-archive"
            '';
          });

      niv = import sources.niv { };
    };
  };

  project = haskellPackages.site;
in
{
  project = project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint       # or ormolu
      niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
    ];
    withHoogle = true;

    # Unicode support, TODO: Make that work outside of the shell (wrapProgram, makeWrapper?)
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    shellHook = ''
      export LANG=en_US.UTF-8
    '';
  };
}
