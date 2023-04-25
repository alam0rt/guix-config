(define-module (services tailscale)
	       #:use-module (gnu services)
	       #:use-module (gnu services shepherd)
	       #:use-module (gnu services configuration)
	       #:use-module (packages tailscale)
	       #:export (tailscale-service-type))

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

(define tailscaled-shepherd-service-type
  (list (shepherd-service
        (provision '(tailscale))
	(actions (list %tailscale-up-action))
        (start #~(make-forkexec-constructor
		   (list
		     #$(file-append tailscale "/bin/tailscaled")
		     "--state=foo.state")))
        (stop #~(make-kill-destructor)))))

(define tailscale-service-type
  (service-type
    (name 'tailscale)
    (extensions
      (list (service-extension shepherd-root-service-type
			   tailscaled-shepherd-service-type)))
    (description "Launch tailscale.")))
