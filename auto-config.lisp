(define-configuration buffer
  ((current-zoom-ratio 4)))
(define-configuration browser
  ((external-editor-program '("/usr/bin/emacsclient"))))
(define-configuration browser
  ((session-restore-prompt :never-restore)))
(define-configuration buffer
  ((default-modes (append '(nyxt::emacs-mode) %slot-default%))))
