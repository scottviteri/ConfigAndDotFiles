(use-modules (gnu) (gnu packages) (gnu system) (gnu services xorg))
(use-package-modules shells)
(use-service-modules networking ssh desktop dbus)

(define my-keyboard-layout (keyboard-layout "us" "colemak"))

(operating-system
  (host-name "scottviteri-thinkpad-guixsd")
  (timezone "America/Los_Angeles")
  (locale "en_US.utf8")
  (keyboard-layout my-keyboard-layout)

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sda")
		(keyboard-layout my-keyboard-layout)))
  (file-systems (cons (file-system
                        (device "/dev/sda3")
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (users (cons (user-account
                (name "scottviteri")
                (group "users")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video"))
		(home-directory "/home/scottviteri")
	        (shell #~(string-append #$fish "/bin/fish")))
	 %base-user-accounts))

  (packages (append (map specification->package 
			 (list "sudo""dmenu" "sway" "wofi" 
			       "gnome-terminal" "wpa-supplicant" 
			       "screen" "emacs" "vim" "git" 
			       "nss-certs" "icecat" "magic-wormhole"
			       "font-gnu-freefont" "font-gnu-freefont-ttf")) 
		    %base-packages))

  (services (cons* 
	      (dbus-service)
	      (service elogind-service-type)
	      (service dhcp-client-service-type)
	      (service openssh-service-type (openssh-configuration (port-number 2222)))
	      (service wpa-supplicant-service-type 
		       (wpa-supplicant-configuration 
			 (interface "wlp2s0")
			 (config-file "/home/scottviteri/.config/wpa_supplicant/Stanford.conf")))
	      %base-services)))
