{
  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";    
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
    flakey = {
      url = "github:zarthross/flakey";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    copilot-el = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
  };
  outputs = { nixpkgs, nixpkgs-unstable, nix-darwin, home-manager, digital-nix, emacs-overlay
    , flakey, copilot-el, ... }@inputs:
    let system = "x86_64-darwin";
    in {
      darwinConfigurations."C02D4849MD6T" = nix-darwin.lib.darwinSystem {
        inherit system;
        modules = [ ];
      };
      homeConfigurations."davidgutsch" =
        home-manager.lib.homeManagerConfiguration {
          modules = [ ./home.nix flakey.homeManagerModules.default ];
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              #	  (final: prev: digital-nix.packages.${system})
              #	  digital-nix.overlays.default
              (final: prev: {
                unstable = nixpkgs-unstable.legacyPackages.${system};
              })
              emacs-overlay.overlay
              flakey.overlays.default
            ];
          };
          # Pass our flake inputs into the config
          extraSpecialArgs = { inherit inputs; };
        };
    };
}
