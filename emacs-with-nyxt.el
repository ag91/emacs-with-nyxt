;;; emacs-with-nyxt.el --- some code to run Nyxt via Emacs.

;; Copyright (C) 2021 Andrea Giugliano

;; Author: Andrea Giugliano <andrea-dev@hotmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; some code to run Nyxt via Emacs
;;
;; some extra docs
;;
;; See documentation on https://github.com/ag91/emacs-with-nyxt

;;; Code:

(require 'slime)
(require 'org)
(require 's)

(defvar emacs-with-nyxt-slime-nyxt-delay
  0.1
  "Delay to wait for Slime commands to reach Nyxt.")

(setq slime-protocol-version 'ignore)

(defun emacs-with-nyxt-slime-connect (host port)
  "Connect Slime to HOST and PORT ignoring version mismatches."
  (slime-connect host port)
  (while (not (slime-connected-p))
    (sleep-for emacs-with-nyxt-slime-nyxt-delay)))

(defun emacs-with-nyxt-slime-repl-send-sexps (&rest s-exps)
  "Evaluate S-EXPS with Nyxt Slime session."
  (let ((s-exps-string (s-join "" (--map (prin1-to-string it) s-exps))))
    (defun true (&rest args) 't)
    (if (slime-connected-p)
        (slime-repl-eval-string s-exps-string)
      (error "Slime is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first"))))

(add-to-list
 'org-capture-templates
 `("wN" "Web link" entry (file+headline ,(car org-agenda-files) "Links to read later")
   "* %?%a :readings: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
   :immediate-finish t :empty-lines 2))

(defun emacs-with-nyxt-start-and-connect-to-nyxt (&optional no-maximize)
  "Start Nyxt with swank capabilities. Optionally skip window maximization with NO-MAXIMIZE."
  (interactive)
  (async-shell-command (format "nyxt -e \"(nyxt-user::start-swank)\""))
  (while (not (ignore-errors (not (emacs-with-nyxt-slime-connect "localhost" "4006"))))
    (sleep-for emacs-with-nyxt-slime-nyxt-delay))
  (while (not (ignore-errors (string= "NYXT-USER" (slime-current-package))))
    (sleep-for emacs-with-nyxt-slime-nyxt-delay))
  (emacs-with-nyxt-slime-repl-send-sexps
   `(load "~/quicklisp/setup.lisp")
   `(defun replace-all (string part replacement &key (test #'char=))
      "Return a new string in which all the occurences of the part is replaced with replacement."
      (with-output-to-string (out)
                             (loop with part-length = (length part)
                                   for old-pos = 0 then (+ pos part-length)
                                   for pos = (search part string
                                                     :start2 old-pos
                                                     :test test)
                                   do (write-string string out
                                                    :start old-pos
                                                    :end (or pos (length string)))
                                   when pos do (write-string replacement out)
                                   while pos)))

   `(defun eval-in-emacs (&rest s-exps)
      "Evaluate S-EXPS with emacsclient."
      (let ((s-exps-string (replace-all
                            (write-to-string
                             `(progn ,@s-exps) :case :downcase)
                            ;; Discard the package prefix.
                            "nyxt::" "")))
        (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
        (uiop:run-program
         (list "emacsclient" "--eval" s-exps-string))))
   `(ql:quickload "cl-qrencode")
   `(define-command-global my/make-current-url-qr-code ()
      "Something else."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png"))
   '(define-command-global my/open-html-in-emacs ()
      "Open buffer html in Emacs."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (eval-in-emacs
       `(progn (switch-to-buffer (get-buffer-create ,(render-url (url (current-buffer))))) (erase-buffer) (insert ,(ffi-buffer-get-document (current-buffer))) (html-mode) (indent-region (point-min) (point-max)))))

   ;; from @aartaka https://www.reddit.com/r/Nyxt/comments/ock3tu/is_there_something_like_mx_or_esc_in_nyxt/h3wkipl?utm_source=share&utm_medium=web2x&context=3
   `(define-command-global eval-expression ()
      "Prompt for the expression and evaluate it, echoing result to the `message-area'."
      (let ((expression-string
             ;; Read an arbitrary expression. No error checking, though.
             (first (prompt :prompt "Expression to evaluate"
                            :sources (list (make-instance 'prompter:raw-source))))))
        ;; Message the thing to the message-area down below.
        (echo "~S" (eval (read-from-string expression-string)))))

   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind eval-expression to M-:, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "M-:" 'eval-expression)
                        scheme))))
   `(define-command-global org-capture ()
      "Org-capture current page."
      (eval-in-emacs
       `(org-link-set-parameters
         "nyxt"
         :store (lambda ()
                  (org-store-link-props
                   :type "nyxt"
                   :link ,(quri:render-uri (url (current-buffer)))
                   :description ,(title (current-buffer)))))
       `(org-capture nil "wN"))
      (echo "Note stored!"))
   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind org-capture to C-o-c, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "C-c o c" 'org-capture)
                        scheme))))
   )
  (unless no-maximize
    (emacs-with-nyxt-slime-repl-send-sexps
     '(toggle-fullscreen))))

(defun emacs-with-nyxt-browse-url-nyxt (url &optional buffer-title)
  "Open URL with Nyxt and optionally define BUFFER-TITLE."
  (interactive "sURL: ")
  (emacs-with-nyxt-slime-repl-send-sexps
   (concatenate
    'list
    (list
     'buffer-load
     url)
    (if buffer-title
        `(:buffer (make-buffer :title ,buffer-title))
      nil))))

(defun emacs-with-nyxt-close-nyxt-connection ()
  "Close Nyxt connection."
  (interactive)
  (emacs-with-nyxt-slime-repl-send-sexps '(quit)))

(defun browse-url-nyxt (url &optional new-window)
  "Browse URL with Nyxt. NEW-WINDOW is ignored."
  (interactive "sURL: ")
  (unless (slime-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-browse-url-nyxt url url))

(defun emacs-with-nyxt-search-first-in-nyxt-current-buffer (string)
  "Search current Nyxt buffer for STRING."
  (interactive "sString to search: ")
  (unless (slime-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-slime-repl-send-sexps
   `(nyxt/web-mode::highlight-selected-hint
     :link-hint
     (car (nyxt/web-mode::matches-from-json
           (nyxt/web-mode::query-buffer :query ,string)))
     :scroll 't)))

(defun emacs-with-nyxt-make-qr-code-of-current-url ()
  "Open QR code of current url."
  (interactive)
  (if (file-exists-p "~/quicklisp/setup.lisp")
      (progn
        (unless (slime-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
        (emacs-with-nyxt-slime-repl-send-sexps
         '(ql:quickload "cl-qrencode")
         '(ql:quickload "cl-qrencode")
         '(cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png"))
        (find-file "/tmp/qrcode.png")
        (auto-revert-mode))
    (error "You cannot use this until you have Quicklisp installed! Check how to do that at: https://www.quicklisp.org/beta/#installation")))

(provide 'emacs-with-nyxt)
;;; emacs-with-nyxt ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
