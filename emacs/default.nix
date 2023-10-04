{pkgs, ...}:
{  
  programs.emacs.custom.package = pkgs.emacs29;
  programs.emacs.custom.extraInit = [
    ./init.el
  ];
}
