;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (guix packages)
             (guix gexp)
             (services samba)
	     (gnu packages)
	     (gnu packages docker)
	     (gnu packages glib)
	     (gnu services)
	     (gnu services dbus)
             (gnu services linux)
             (gnu services syncthing)
             (gnu services nfs)
	     (gnu services docker)
	     (gnu services web)
             (gnu services file-sharing))


(use-service-modules desktop networking web ssh xorg shepherd file-sharing docker dbus)

(use-package-modules file-systems linux containers)

(define config-kernel linux-libre-5.17)

(define compile-zfs
  (package
    (inherit zfs)
    (arguments
      (cons* #:linux config-kernel
             (package-arguments zfs)))))

(define zfs-shepherd-services
  (let ((zpool            (file-append compile-zfs "/sbin/zpool"))
        (zfs              (file-append compile-zfs "/sbin/zfs"))
        (scheme-modules   `((srfi srfi-1)
                            (srfi srfi-34)
                            (srfi srfi-35)
                            (rnrs io ports)
                            ,@%default-modules)))
    (define zfs-scan 
      (shepherd-service
        (provision '(zfs-scan))
        (documentation "Scans for ZFS pools.")
        (requirement '(kernel-module-loader udev))
        (modules scheme-modules)
        (start #~(lambda _
                   (invoke/quiet #$zpool "import" "-a" "-N")))
        (stop #~(const #f))))
    (define zfs-automount
      (shepherd-service
        (provision '(zfs-automount))
        (documentation "Automounts ZFS data sets.")
        (requirement '(zfs-scan))
        (modules scheme-modules)
        (start #~(lambda _
                   (with-output-to-port
                     (current-error-port)
                     (lambda ()
                       (invoke #$zfs "mount" "-a" "-l")))))
        (stop #~(lambda _
                  (chdir "/") 
                  (invoke/quiet #$zfs "unmount" "-a" "-f")
                  #f))))
    (list zfs-scan   
          zfs-automount)))

  

(operating-system
  (kernel config-kernel)
  (kernel-loadable-modules (list (list compile-zfs "module")))
  (locale "en_AU.utf8")
  (timezone "Australia/Melbourne")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "sauron")
  (users (cons* (user-account
                  (name "sam")
                  (comment "Sam")
                  (group "users")
                  (home-directory "/home/sam")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "docker")))
                (user-account
                  (name "foo")
                  (comment "UNIX account for logins to Samba")
                  (group "users")
                  (shell "/bin/nologin")
                  (create-home-directory? #f))
                %base-user-accounts))
  (packages
    (append
      (list compile-zfs
            zfs-auto-snapshot
            containerd
	    docker
	    dbus
	    docker-cli
            (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service openssh-service-type
            ;; SSH
              (openssh-configuration
		(password-authentication? #false)))
            ;; NFS
            (service nfs-service-type
              (nfs-configuration
                (nfsd-threads 16)
                (nfsd-udp? #t)
                (exports
                  '(("/mordor/share"
                     "192.168.0.0/24(rw,sync,crossmnt,fsid=0)")
                    ("/mordor/share/sam"
                     "192.168.0.0/24(rw,sync)")
                    ("/tmp"
                     "192.168.0.0/24(rw,sync)")
                    ("/mordor/downloads"
                     "192.168.0.0/24(rw,all_squash,anonuid=65534,anongid=997)")))))
            ;; Syncthing
            (service syncthing-service-type
              (syncthing-configuration
                (user "sam")
                (home "/mordor/foo")))
            ;; Samba
            (service samba-service-type
              (samba-configuration
                (config-file (plain-file "smb.conf"
                 "[global]
                    workgroup = MORDOR
                    server string = Samba Server
                    server role = standalone server
                    log file = /var/log/samba/log.%m
                    logging = file
                    guest account = nobody
                    security = user

                  [downloads]
                    comment = Download directory
                    path = /mordor/downloads
                    public = yes
                    read only = yes
                    printable = no
                    guest ok = yes

                  [public]
                    comment = Public directory
                    path = /mordor/share/public
                    public = yes
                    writeable = yes
                    guest ok = yes
              "))))
            (service wsdd-service-type
              (wsdd-configuration
               (workgroup "MORDOR")))
	    ;; Docker
	    (dbus-service)
	    (elogind-service)
	    (service docker-service-type)
	    ;; NGINX
	    (service nginx-service-type
	      (nginx-configuration
		(server-blocks
		  (list (nginx-server-configuration
		     	 (server-name '("mordor"))
			  (listen '("80"))
			  (root "/dev/null")
			  (raw-content '("deny all;"
					 "allow 192.168.0.0/24;"
	        			 "allow 173.245.48.0/20;"
		       		         "allow 103.21.244.0/22;"
		 	               	 "allow 103.22.200.0/22;"
					 "allow 103.31.4.0/22;"
					 "allow 141.101.64.0/18;"
					 "allow 108.162.192.0/18;"
					 "allow 190.93.240.0/20;"
					 "allow 188.114.96.0/20;"
					 "allow 197.234.240.0/22;"
					 "allow 198.41.128.0/17;"
					 "allow 162.158.0.0/15;"
					 "allow 104.16.0.0/13;"
					 "allow 104.24.0.0/14;"
					 "allow 172.64.0.0/13;"
					 "allow 131.0.72.0/22;"))
			  (locations
			    (list
			      (nginx-location-configuration
			        (uri "/")
			        (body '("proxy_pass http://server-proxy;"))))))))
		(upstream-blocks
		  (list (nginx-upstream-configuration
			  (name "server-proxy")
			  (servers (list "localhost:8096")))))))
            ;; Torrents
            (service transmission-daemon-service-type
              (transmission-daemon-configuration
                (download-dir "/mordor/downloads")
                ;; Restrict access to the RPC ("control") interface
                (rpc-authentication-required? #t)
                (rpc-username "transmission")
                (rpc-password
                  (transmission-password-hash
                    '()     ; desired password
                    "foo"))   ; arbitrary salt value
                ;; Accept requests from this and other hosts on the
                ;; local network
                (rpc-whitelist-enabled? #t)
                (rpc-whitelist '("::1" "127.0.0.1" "192.168.0.*"))))
            ;; DHCP
            (service dhcp-client-service-type)
            ;; Time
            (service ntp-service-type)
            ;; Docker services
	    (simple-service 'docker-shepherd-services
			    shepherd-root-service-type
			    docker-shepherd-services)
	    (simple-service 'docker-shepherd-services
			    user-processes-service-type
			    '(jellyfin))
	    ;; ZFS
            (simple-service 'zfs-loader
                            kernel-module-loader-service-type
                            '("zfs"))
            (simple-service 'zfs-shepherd-services
                             shepherd-root-service-type
                             zfs-shepherd-services)
            (simple-service 'zfs-sheperd-services-user-processes
                             user-processes-service-type
                             '(zfs-automount)))
      %base-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid ""))
            (target "cryptroot")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           %base-file-systems)))

