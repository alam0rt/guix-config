;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
	     (gnu services shepherd)
	     (gnu services security-token)
	     (services tailscale)
	     (packages tailscale)
	     (nongnu packages linux))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "en_AU.utf8")
  (timezone "Australia/Melbourne")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "sanic")
  (kernel linux)
  (firmware (list linux-firmware))

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "sam")
                  (comment "Sam Lockart")
                  (group "users")
                  (home-directory "/home/sam")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "cdrom" "lp")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (specifications->packages (list "tailscale"
						    "bluez"
						    "nss-certs"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service xfce-desktop-service-type)
                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
		 (bluetooth-service #:auto-enable? #t)
		 (simple-service 'etc-subuid etc-service-type
				 (list `("subuid" ,(plain-file "subuid" "sam:100000:65536\n"))))
		 (simple-service 'etc-subgid etc-service-type
				 (list `("subgid" ,(plain-file "subgid" "sam:100000:65536\n"))))
		 ;; Support communicating with YubiKey as a SmartCard device.
		 (service pcscd-service-type)
                 (service openssh-service-type)
		 (service tailscaled-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))
           ;; This is the default list of services we
           ;; are appending to.
           (modify-services %desktop-services
             (guix-service-type config => (guix-configuration
               (inherit config)
               (substitute-urls
                (append (list "https://substitutes.nonguix.org")
                  %default-substitute-urls))
               (authorized-keys
                (append (list (plain-file "non-guix.pub"
					  "(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )
"))
                  %default-authorized-guix-keys))))
             (elogind-service-type
               config =>
                 (elogind-configuration
		   (inherit config)
                   (handle-power-key 'suspend)
                   (handle-lid-switch-external-power 'suspend))))))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "5f53aa45-5b89-43c8-a156-1364e48b50ed"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "DEAD-BEEF"
                                       'fat16))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems)))
