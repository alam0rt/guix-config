(define-module (services tailscale)
	       #:use-module (gnu services)
	       #:use-module (gnu services shepherd)
	       #:use-module (gnu services configuration)
	       #:use-module (guix records)
	       #:use-module (guix gexp)
	       #:use-module (ice-9 match)
	       #:use-module (packages tailscale)
	       #:export (tailscale-service tailscale-service-type tailscale-configuration))

(define-record-type* <tailscale-configuration>
		     tailscale-configuration make-tailscale-configuration
		     tailscale-configuration?
		     (tailscale tailscale-configuration-tailscale
				(default tailscale))
		     (state-file tailscale-configuration-state-file
				 (default "foo.state")))

(define %tailscale-up-action
  (shepherd-action
    (name 'up)
    (documentation "Connect your machine to the tailscale network.")
    (procedure #~(lambda (running)
		   (let* ((tailscale-cli (string-append #$tailscale "/bin/tailscale"))
			  (cmd (string-join (list tailscale-cli "up")))
			  (port (open-input-pipe cmd))
			  (str (get-string-all port)))
		     (display str)
		     (status:exit-val (close-pipe port)))))))

(define tailscaled-shepherd-service
  (match-lambda
    (($ <tailscale-configuration> tailscale state-file)
       (list (shepherd-service
         (provision '(tailscale))
	 (requirement '()) ;; services this depends on
	 (actions (list %tailscale-up-action))
         (start #~(make-forkexec-constructor
	 	   (list
	 	     #$(file-append tailscale "/bin/tailscaled")
 		     "--state" #$state-file)))
         (stop #~(make-kill-destructor)))))))

(define tailscale-service-type
  (service-type
    (name 'tailscale)
    (default-value (tailscale-configuration))
    (extensions
      (list (service-extension shepherd-root-service-type
			   tailscaled-shepherd-service)))
    (description "Launch tailscale.")))

(define* (tailscale-service #:key (tailscale tailscale))
	 "tailscale service"
	 (service tailscale-service-type
		  (tailscale-configuration
		    (tailscale tailscale))))
