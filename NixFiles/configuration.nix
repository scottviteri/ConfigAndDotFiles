# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
#  boot.loader.grub = {
#   enable = true;
#   version = 2;
#   device = "/dev/sda";
#   extraEntries 
#  };


   networking.hostName = "scottviteri"; # Define your hostname.
   networking.networkmanager.enable = true;
  
  services.xserver.windowManager.i3.enable = true;

  nixpkgs.config.allowUnfree = false;
  services.xserver.libinput.enable = true;
  
  environment.systemPackages =  with pkgs; [
   vim
   git
   which
   tor 
   gnome3.gnome_terminal
   zathura
   firefox
   tmux
   curl
   zsh
   i3 i3lock i3status dmenu
  ] ++ (with haskellPackages; [
   ghc
  ]);

  # Select internationalisation properties. 
  i18n = {
     consoleKeyMap = "us"; defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone. time.timeZone = "Europe/Amsterdam";
  time.timeZone = "US/Eastern";

  # List packages installed in system profile. To search by name, run: $ nix-env -qaP | grep wget environment.systemPackages = with pkgs; [
  #   wget ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon. 
  services.openssh.enable = true;

  # Open ports in the firewall. networking.firewall.allowedTCPPorts = [ ... ]; networking.firewall.allowedUDPPorts = [ ... ]; Or disable the firewall altogether. networking.firewall.enable = false;

  # Enable CUPS to print documents. services.printing.enable = true;

  # Enable the X11 windowing system. 
services.xserver.enable = true; services.xserver.layout = "us"; services.xserver.xkbOptions = "eurosign:e";
services.xserver.displayManager.sessionCommands =  '' ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap '';

nix.gc.automatic = true;

programs.zsh.enable = true;

environment.sessionVariables = { 
 EDITOR = "vim";
 NIXPKGS_ALLOW_UNFREE = "0";
 PYTHONDONTWRITEBYTECODE = "1";
};
  # Define a user account. Don't forget to set a password with ‘passwd’. users.extraUsers.guest = {
  #   isNormalUser = true; uid = 1000;
  # };
  #

  security.sudo = {
   enable = true;
   configFile = ''
    Defaults env_reset
    root ALL = (ALL:ALL) ALL
    wheel ALL = (ALL) SETENV: ALL
   '';
   wheelNeedsPassword = false;
  };
 
  security.initialRootPassword = ... ;

  users = {
   extraUsers.scottviteri = {
    isNormalUser = true;
    password = ... ;
    uid = 1000; 
    createHome = true;
    home = "/home/scottviteri";
    extraGroups = [ "wheel" "networkmanager" ];
   };
   defaultUserShell = "/run/current-system/sw/bin/zsh";
   mutableUsers = false;
  };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
  system.autoUpgrade.enable = true;

}
