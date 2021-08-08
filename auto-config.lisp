(define-configuration buffer
  ((current-zoom-ratio 4)))
(define-configuration browser
  ((external-editor-program '("/usr/bin/emacsclient"))))
(define-configuration browser
  ((session-restore-prompt :never-restore)))
(define-configuration buffer
  ((default-modes (append '(nyxt::emacs-mode) %slot-default%))))

(define-configuration buffer
  ((request-resource-hook (reduce #'hooks:add-hook
                                  (list (url-dispatching-handler
                                         'doi-link-dispatcher
                                         (match-scheme "doi")
                                         (lambda (url)
                                           (quri:uri (format nil "https://doi.org/~a"
                                                             (quri:uri-path url)))))
                                        (url-dispatching-handler
                                         'transmission-magnet-links
                                         (match-scheme "magnet")
                                         "transmission-remote --add ~a")
                                        ;; (url-dispatching-handler
                                        ;;  'emacs-file
                                        ;;  (match-scheme "file")
                                        ;;  (lambda (url)
                                        ;;    (uiop:launch-program
                                        ;;     `("emacs" ,(quri:uri-path url)))
                                        ;;    nil))
                                        (url-dispatching-handler
                                         'emacs-mail
                                         (match-scheme "mailto")
                                         "/usr/bin/emacsclient --eval '(browse-url-mail \"~a\")'"
                                         ))
                                  :initial-value %slot-default%))))

(load "~/quicklisp/setup.lisp")
(ql:quickload :slynk)
(define-command-global start-slynk (&optional (slynk-port *swank-port*))
    "Start a Slynk server that can be connected to, for instance, in
Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to execute
arbitrary code with the privileges of the user running Nyxt.  Make sure
you understand the security risks associated with this before running
this command."
    (slynk:create-server :port slynk-port :dont-close t)
    (echo "Slynk server started at port ~a" slynk-port))
(start-slynk)
