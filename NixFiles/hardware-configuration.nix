# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems = {
   "/" =
    { device = "/dev/sda5";
      fsType = "ext4";
    };

   "/mnt" =
    { device = "/dev/sda3";
      fsType = "ext4";
    };

   "/boot" =
    { device = "/dev/sda1";
      fsType = "vfat";
    };
  };

  swapDevices = [ {device = "/dev/sda2";} ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = "powersave";
}
