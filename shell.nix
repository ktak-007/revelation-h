{ sys ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
}:

let
  name = "Revelation-H";
  usage = "Usage:\tcabal build\n\tghcid --command 'cabal repl'";

  pkgs = import sources.nixpkgs {};

  haskell = pkgs.haskellPackages;
  haskellWithPackages = haskell.ghcWithPackages (hpkgs: with hpkgs; [
    haskell-gi-base
    gi-adwaita
    gi-gtk
    gi-gio
  ]);

  projectPackages = (with haskellWithPackages; [
    haskellWithPackages
    haskell.ghcid
    haskell.cabal-install
    haskell.haskell-language-server
  ]) ++ (if pkgs.stdenv.isLinux then [
    pkgs.xclip # For Hclip library
  ] else []);

  funPackages = with sys; [
    figlet
    boxes
    lolcat
    onefetch
  ];

in
  sys.mkShell {
    packages = projectPackages ++ [
      sys.zlib
    ]
    ++ funPackages;

    # inputsFrom = [(import ./default.nix {}).app.env];
    # nativeBuildInputs = [ sys.zlib ];

    shellHook = ''
      echo
      echo "${name}" | figlet -t -f banner3-D | lolcat -f
      onefetch
      echo -e "${name} development shell\n\n${usage}" | boxes -d parchment | lolcat -f
      echo
  '';

  }
