{ inputs, config, pkgs, ... }:
let initFile = ./init.el;
in {
  home.packages = [
    pkgs.nodejs
    pkgs.nixfmt
    pkgs.scalafmt
    pkgs.nodePackages_latest.pyright
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs;
      override = epkgs:
        epkgs // {
          copilot = let
            copilot-lisp = epkgs.trivialBuild {
              pname = "copilot-lisp";
              src = inputs.copilot-el;
              packageRequires = [ epkgs.dash epkgs.editorconfig epkgs.s ];
            };
            copilot-dist = pkgs.stdenv.mkDerivation {
              name = "copilot-dist";
              src = inputs.copilot-el;
              installPhase =
                "  LISPDIR=$out/share/emacs/site-lisp\n  mkdir -p $LISPDIR\n  cp -R dist $LISPDIR\n";
            };
          in pkgs.symlinkJoin {
            name = "emacs-copilot";
            paths = [ copilot-lisp copilot-dist ];
          };
        };

      config = initFile;
      defaultInitFile = false;
      alwaysEnsure = true;
    };
    extraPackages = epkgs: [ epkgs.use-package ];
  };

  xdg.configFile = {
    "emacs/init.el".source = initFile;
    "emacs/early-init.el".source = ./early-init.el;
    "emacs/load-path.el".source = pkgs.writeText "load-path.el" ''
              (let ((default-directory (file-name-as-directory
      				"${config.programs.emacs.package.deps}/share/emacs/site-lisp/"))
      	    (normal-top-level-add-subdirs-inode-list nil))
            (normal-top-level-add-subdirs-to-load-path))
    '';
  };
}
