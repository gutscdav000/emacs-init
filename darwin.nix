{ config, pkgs, ... }:

{
  jkhy.digital.security.pki.enable = true;
  services.nix-daemon.enable = true;
}
