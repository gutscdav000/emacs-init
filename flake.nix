{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";
    nix-darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    digital-nix = {
      url = "git+ssh://git@github.com/banno/digital-nix.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };
  outputs = {nixpkgs, nix-darwin, home-manager, digital-nix, emacs-overlay, ...}:
  let system = "x86_64-darwin";
  in {
    darwinConfigurations."C02D4849MD6T" = nix-darwin.lib.darwinSystem {
      inherit system;
      modules = [];
    };
    homeConfigurations."davidgutsch" = home-manager.lib.homeManagerConfiguration {
      modules = [./home.nix];
      pkgs = import nixpkgs{
        inherit system;
        overlays = [
#	  (final: prev: digital-nix.packages.${system})
#	  digital-nix.overlays.default
	  emacs-overlay.overlay
	];
      };
    };
  };
}