{ config, pkgs, ... }:

{
  imports = [ ./emacs ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "davidgutsch";
  home.homeDirectory = "/Users/davidgutsch";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  jkhy.digital.programs.beelzebub.enable = true;
  jkhy.digital.programs.postgresConnect.enable = false;
#  jkhy.digital.programs.gcp.enable = false;

  home.packages = with pkgs; [

    redis
    # tools
    k9s
    silver-searcher
    ripgrep
    #postgres-connect
    # programming languages
    python3
    unstable.metals
    unstable.bloop
    scala-cli
    # desktop apps
    hot
    keepingYouAwake
    rectangle
    # Git
    ghorg
    gh    
  ];

}
