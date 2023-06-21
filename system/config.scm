;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.
;; Indicate which modules to import to access the variables
;; used in this configuration.
(set! (@ (gnu system file-systems) %control-groups) '())
(set! (@ (gnu system file-systems) %elogind-file-systems) '())
(define elogind-dbus-service (@@ (gnu services desktop) elogind-dbus-service))
(define elogind-package (@@ (gnu services desktop) elogind-package))
(define elogind-shepherd-service (@@ (gnu services desktop) elogind-shepherd-service))
(define pam-extension-procedure (@@ (gnu services desktop) pam-extension-procedure))

(use-modules (gnu services desktop)
	     (gnu services dbus)
	     (gnu services base)
	     (gnu system pam)
	     (gnu services shepherd)
	     (gnu system file-systems))

(define %elogind-file-systems-v2
  ;; We don't use systemd, but these file systems are needed for elogind,
  ;; which was extracted from systemd.
   (list (file-system
           (device "none")
           (mount-point "/run/systemd")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
         (file-system
           (device "none")
           (mount-point "/run/user")
           (type "tmpfs")
           (check? #f)
           (flags '(no-suid no-dev no-exec))
           (options "mode=0755")
           (create-mount-point? #t))
	 (file-system
           (device "none")
	   (mount-point "/sys/fs/cgroup")
	   (type "cgroup2")
	   (check? #f)
	   (create-mount-point? #f))
         ;; Elogind uses cgroups to organize processes, allowing it to map PIDs
         ;; to sessions.  Elogind's cgroup hierarchy isn't associated with any
         ;; resource controller ("subsystem").
         (file-system
           (device "cgroup")
           (mount-point "/sys/fs/cgroup/elogind")
           (type "cgroup")
           (check? #f)
           (options "none,name=elogind")
           (create-mount-point? #t))))

(define elogind-service-v2-type
    (service-type (name 'elogind2)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          elogind-dbus-service)
                       (service-extension udev-service-type
                                          (compose list elogind-package))
                       (service-extension polkit-service-type
                                          (compose list elogind-package))

                       ;; Start elogind from the Shepherd rather than waiting
                       ;; for bus activation.  This ensures that it can handle
                       ;; events like lid close, etc.
                       (service-extension shepherd-root-service-type
                                          elogind-shepherd-service)

                       ;; Provide the 'loginctl' command.
                       (service-extension profile-service-type
                                          (compose list elogind-package))

                       ;; Extend PAM with pam_elogind.so.
                       (service-extension pam-root-service-type
                                          pam-extension-procedure)

                       ;; We need /run/user, /run/systemd, etc.
                       (service-extension file-system-service-type
                                          (const %elogind-file-systems-v2))))
                (default-value (elogind-configuration))
                (description "Run the @command{elogind} login and seat
management service.  The @command{elogind} service integrates with PAM to
allow other system components to know the set of logged-in users as well as
their session types (graphical, console, remote, etc.).  It can also clean up
after users when they log out.")))


(use-modules (gnu)
	     (gnu services shepherd)
	     (gnu services dbus)
	     (gnu services security-token)
	     (services tailscale)
	     (packages tailscale)
	     (gnu system file-systems)
	     (srfi srfi-1)
	     (nongnu packages linux))


(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "en_AU.utf8")
  (timezone "Australia/Melbourne")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "sanic")
  (kernel linux)
  (kernel-arguments (cons "systemd.unified_cgroup_hierarchy=1" %default-kernel-arguments)) ; enable cgroups v2
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
						    "nfs-utils"
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
				 (list `("subuid" ,(plain-file "subuid" "root:0:65536\nsam:100000:65536\n"))))
		 (simple-service 'etc-subgid etc-service-type
				 (list `("subgid" ,(plain-file "subgid" "root:0:65536\nsam:100000:65536\n"))))
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
             (elogind-service-v2-type
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
                         (dependencies mapped-devices))  %base-file-systems)))
