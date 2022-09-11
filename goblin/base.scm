;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
	     (guix transformations)
	     (gnu packages xorg)
	     (nongnu packages linux)
	     (nongnu packages nvidia)
	     (nongnu system linux-initrd))

(use-service-modules
 desktop
 networking
 linux
 ssh
 xorg)

(use-package-modules package-management)

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

(define %xorg-config
    "Section \"Device\"
        Identifier     \"Device0\"
        Driver         \"nvidia\"
        VendorName     \"NVIDIA Corporation\"
        BoardName      \"GeForce GTX 960\"
    EndSection
")

(operating-system
  (kernel linux-lts)
  (initrd microcode-initrd)
  (kernel-arguments '("modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma,nouveau"))
  (kernel-loadable-modules (list broadcom-sta nvidia-driver))
  (firmware (cons* broadcom-bt-firmware
                   %base-firmware))
  (locale "en_AU.utf8")
  (timezone "Australia/Melbourne")
  (keyboard-layout (keyboard-layout "au"))
  (host-name "goblin")
  (users (cons* (user-account
                  (name "sam")
                  (comment "Sam")
                  (group "users")
                  (home-directory "/home/sam")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
	    (simple-service 
                     'custom-udev-rules udev-service-type 
                     (list nvidia-driver))
            (service kernel-module-loader-service-type
                     '("ipmi_devintf"
                       "nvidia"
                       "nvidia_modeset"
                       "nvidia_uvm"))
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "a655d0ce-f200-4e88-b4b1-51005caf6e2a")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "7E5E-030E" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "8ae5b1f6-d981-4696-bfe7-64e08e2e4f2c"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
