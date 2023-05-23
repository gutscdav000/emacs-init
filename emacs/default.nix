{ config, pkgs, ... }:
let initFile = ./init.el;
in 
{
  programs.emacs = {
        enable = true;
        package = pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacs;

          config = initFile;
          defaultInitFile = false;
          alwaysEnsure = true;
        };
        extraPackages = epkgs: [
          epkgs.use-package
        ];
      };

      xdg.configFile = {
        "emacs/init.el".text = initFile;
        "emacs/early-init.el".source = ./early-init.el;
      };
}