(define-module (services tailscale)
	       #:use-module (gnu services)
	       #:use-module (gnu services shepherd)
	       #:use-module (gnu services configuration)
	       #:use-module (guix records)
	       #:use-module (guix gexp)
	       #:use-module (ice-9 match)
	       #:use-module (packages tailscale)
	       #:use-module (gnu packages linux)
	       #:export (tailscale-service-type tailscale-configuration))

(define-record-type* <tailscale-configuration>
		     tailscale-configuration make-tailscale-configuration
		     tailscale-configuration?
		     (tailscale tailscale-configuration-tailscale
				(default tailscale))
		     (state-file tailscale-configuration-state-file
				 (default "tailscaled.state")))

(define %tailscale-up-action
  (shepherd-action
    (name 'up)
    (documentation "Connect your machine to the tailscale network.")
    (procedure #~(lambda (running)
		   (let* ((tailscale-cli (string-append #$tailscale "/usr/bin/tailscale"))
			  (cmd (string-join (list tailscale-cli "up")))
			  (port (open-input-pipe cmd))
			  (str (get-string-all port)))
		     (display str)
		     (status:exit-val (close-pipe port)))))))

(define (tailscale-shepherd-service config)
  "Return a <shepherd-service> for Tailscale with CONFIG"
  (let ((tailscale
	   (tailscale-configuration-tailscale config))

	 (state-file
	   (tailscale-configuration-state-file config)))
     (list
       (shepherd-service
	     (provision '(tailscale))
	     (requirement '(tailscaled))
             (actions (list %tailscale-up-action))
	     (start #~(make-forkexec-constructor
			(list
			  #$(file-append tailscale "/usr/bin/tailscale") "up")))
	     (stop #~(make-kill-destructor))))))


(define (tailscaled-activation config)
  "Run tailscaled --cleanup"
  #~(begin
      (system* #$(file-append tailscale "/usr/bin/tailscaled") "--cleanup")))



(define (tailscaled-shepherd-service config)
  "Return a <shepherd-service> for Tailscaled with CONFIG"
  (let ((tailscale
	   (tailscale-configuration-tailscale config))
	 (state-file
	   (tailscale-configuration-state-file config))
	(environment #~(list (string-append
			       "PATH="
			       (string-append #$iptables "/sbin")
			       ":"
			       (string-append #$iptables "/bin")))))

      (list
	(shepherd-service
         (provision '(tailscaled))
	 (requirement '(networking)) ;; services this depends on
         (start #~(make-forkexec-constructor
		    (list #$(file-append tailscale "/usr/bin/tailscaled")
 		     "-state" #$state-file
		     "-verbose" "10")
		    #:environment-variables #$environment
		    #:log-file "/var/log/tailscaled.log"))
         (stop #~(make-kill-destructor))))))

(define tailscale-service-type
  (service-type
    (name 'tailscale)
    (extensions
      (list (service-extension shepherd-root-service-type
			       tailscaled-shepherd-service)
            (service-extension activation-service-type
			       tailscaled-activation)))
    (default-value (tailscale-configuration))
    (description "Launch tailscale.")))
