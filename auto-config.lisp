(define-configuration (buffer web-buffer)
    ((default-modes (append '(emacs-mode) %slot-default))))
(define-configuration buffer
    ((current-zoom-ratio 4)))
(define-configuration browser
    ((external-editor-program "/usr/bin/emacsclient")))
(define-configuration browser
    ((session-restore-prompt :never-restore)))
