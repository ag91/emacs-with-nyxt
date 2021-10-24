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

;; (require 'slime)
(require 'org)
(require 's)
;; (require 'sly)

(defcustom cl-ide 'sly
  "What IDE to use to evaluate Common Lisp.
Defaults to Sly because it has better integration with Nyxt."
  :options (list 'sly 'slime))

(defvar emacs-with-nyxt-delay
  0.1
  "Delay to wait for `cl-ide' commands to reach Nyxt.")

(setq slime-protocol-version 'ignore)

(defun emacs-with-nyxt-connected-p ()
  "Is `cl-ide' connected to nyxt."
  (cond
   ((eq cl-ide 'slime) (slime-connected-p))
   ((eq cl-ide 'sly) (sly-connected-p)))) ;; TODO this should check it
                                          ;; is connected to Nyxt and
                                          ;; not just to cl-ide
                                          ;; session

(defun emacs-with-nyxt--connect (host port)
  "Connect `cl-ide' to HOST and PORT."
  (cond
   ((eq cl-ide 'slime) (slime-connect host port))
   ((eq cl-ide 'sly) (sly-connect host port))))

(defun emacs-with-nyxt-connect (host port)
  "Connect `cl-ide' to HOST and PORT ignoring version mismatches."
  (emacs-with-nyxt--connect host port)
  (while (not (emacs-with-nyxt-connected-p))
    (message "Starting %s connection..." cl-ide)
    (sleep-for emacs-with-nyxt-delay)))

(defun emacs-with-nyxt-eval (string)
  "Send STRING to `cl-ide'."
  (cond
   ((eq cl-ide 'slime) (slime-repl-eval-string string))
   ((eq cl-ide 'sly) (sly-eval `(slynk:interactive-eval-region ,string)))))

(defun emacs-with-nyxt-send-sexps (&rest s-exps)
  "Evaluate S-EXPS with Nyxt `cl-ide' session."
  (let ((s-exps-string (s-join "" (--map (prin1-to-string it) s-exps))))
    (defun true (&rest args) 't)
    (if (emacs-with-nyxt-connected-p)
        (emacs-with-nyxt-eval s-exps-string)
      (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide)))))

(add-to-list
 'org-capture-templates
 `("wN" "Web link" entry (file+headline ,(car org-agenda-files) "Links to read later")
   "* TODO %?%a :readings: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
   :immediate-finish t :empty-lines 2))

(defun on/slug-string (title)  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                                                        768 ; U+0300 COMBINING GRAVE ACCENT
                                                        769 ; U+0301 COMBINING ACUTE ACCENT
                                                        770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                                        771 ; U+0303 COMBINING TILDE
                                                        772 ; U+0304 COMBINING MACRON
                                                        774 ; U+0306 COMBINING BREVE
                                                        775 ; U+0307 COMBINING DOT ABOVE
                                                        776 ; U+0308 COMBINING DIAERESIS
                                                        777 ; U+0309 COMBINING HOOK ABOVE
                                                        778 ; U+030A COMBINING RING ABOVE
                                                        780 ; U+030C COMBINING CARON
                                                        795 ; U+031B COMBINING HORN
                                                        803 ; U+0323 COMBINING DOT BELOW
                                                        804 ; U+0324 COMBINING DIAERESIS BELOW
                                                        805 ; U+0325 COMBINING RING BELOW
                                                        807 ; U+0327 COMBINING CEDILLA
                                                        813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                                        814 ; U+032E COMBINING BREVE BELOW
                                                        816 ; U+0330 COMBINING TILDE BELOW
                                                        817 ; U+0331 COMBINING MACRON BELOW
                                                        )))
                                 (cl-flet* ((nonspacing-mark-p (char)
                                                               (memq char slug-trim-chars))
                                            (strip-nonspacing-marks (s)
                                                                    (ucs-normalize-NFC-string
                                                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                                                 (ucs-normalize-NFD-string s)))))
                                            (cl-replace (title pair)
                                                        (replace-regexp-in-string (car pair) (cdr pair) title)))
                                   (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                                                   ("__*" . "_") ;; remove sequential underscores
                                                   ("^_" . "") ;; remove starting underscore
                                                   ("_$" . ""))) ;; remove ending underscore
                                          (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
                                     (downcase slug)))))

(defun on/make-filepath (title now &optional zone)
  "Make filename from note TITLE and NOW time (assumed in the current time ZONE)."
  (concat
   org-roam-directory
   (format-time-string "%Y%m%d%H%M%S_" now (or zone (current-time-zone)))
   (s-truncate 70 (on/slug-string title) "")
   ".org"))

(defun on/insert-org-roam-file (file-path title &optional links sources text quote)
  "Insert org roam file in FILE-PATH with TITLE, LINKS, SOURCES, TEXT, QUOTE."
  (with-temp-file file-path
    (insert
     "* " title "\n"
     "\n"
     "- tags :: " (--reduce (concat acc ", " it) links) "\n"
     (if sources (concat "- source :: " (--reduce (concat acc ", " it) sources) "\n") "")
     "\n"
     (if text text "")
     "\n"
     "\n"
     (if quote
         (concat "#+begin_src text \n"
                 quote "\n"
                 "#+end_src")
       "")))
  (with-file file-path
             (org-id-get-create)
             (save-buffer)))

(defun emacs-with-nyxt-current-package ()
  "Return current package set for `cl-ide'."
  (cond
   ((eq cl-ide 'slime) (slime-current-package))
   ((eq cl-ide 'sly) (with-current-buffer (sly-mrepl--find-buffer) (sly-current-package)))))

(defun emacs-with-nyxt-start-and-connect-to-nyxt (&optional no-maximize)
  "Start Nyxt with swank capabilities. Optionally skip window maximization with NO-MAXIMIZE."
  (interactive)
  (async-shell-command (format "nyxt -e \"(nyxt-user::start-swank)\""))
  (while (not (ignore-errors (not (emacs-with-nyxt-connect "localhost" "4006"))))
    (message "Starting Swank connection...")
    (sleep-for emacs-with-nyxt-delay))
  (while (not (ignore-errors (string= "NYXT-USER" (upcase (emacs-with-nyxt-current-package)))))
    (progn (message "Setting %s package to NYXT-USER..." cl-ide)
           (sleep-for emacs-with-nyxt-delay)))
  (emacs-with-nyxt-send-sexps
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
   `(define-command-global my/make-current-url-qr-code () ; this is going to be redundant: https://nyxt.atlas.engineer/article/qr-url.org
      "Something else."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (progn
        (cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png")
        (uiop:run-program (list "nyxt" "/tmp/qrcode.png"))))
   '(define-command-global my/open-html-in-emacs ()
      "Open buffer html in Emacs."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (with-open-file
       (file "/tmp/temp-nyxt.html" :direction :output
             :if-exists :supersede
             :if-does-not-exist :create)
       (write-string (ffi-buffer-get-document (current-buffer)) file))
      (eval-in-emacs
       `(progn (switch-to-buffer
                (get-buffer-create ,(render-url (url (current-buffer)))))
               (erase-buffer)
               (insert-file-contents-literally "/tmp/temp-nyxt.html")
               (html-mode)
               (indent-region (point-min) (point-max))))
      (delete-file "/tmp/temp-nyxt.html"))

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
   `(defun emacs-with-nyxt-capture-link ()
      (let ((url (quri:render-uri (url (current-buffer)))))
        (if (str:containsp "youtu" url)
            (str:concat
             url
             "&t="
             (write-to-string
              (floor
               (ffi-buffer-evaluate-javascript (current-buffer)
                                               (ps:ps
                                                (ps:chain document
                                                          (get-element-by-id "movie_player")
                                                          (get-current-time))))))
             "s")
          url)))
   `(define-command-global org-capture ()
      "Org-capture current page."
      (eval-in-emacs
       `(let ((org-link-parameters
               (list (list "nyxt"
                           :store
                           (lambda ()
                             (org-store-link-props
                              :type "nyxt"
                              :link ,(emacs-with-nyxt-capture-link)
                              :description ,(title (current-buffer))))))))
          (org-capture nil "wN"))
       (echo "Note stored!")))
   `(define-command-global org-roam-capture ()
      "Org-capture current page."
      (let ((quote (%copy))
            (title (prompt
                    :input (title (current-buffer))
                    :prompt "Title of note:"
                    :sources (list (make-instance 'prompter:raw-source))))
            (text (prompt
                   :input ""
                   :prompt "Note to take:"
                   :sources (list (make-instance 'prompter:raw-source))))
            (link (emacs-with-nyxt-capture-link)))
        (eval-in-emacs
         `(let ((file (on/make-filepath ,(car title) (current-time))))
            (on/insert-org-roam-file
             file
             ,(car title)
             nil
             (list ,link)
             ,(car text)
             ,quote)
            (find-file file)
            (org-id-get-create)))
        (echo "Org Roam Note stored!")))
   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind org-capture to C-o-c, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "C-c o c" 'org-capture)
                        scheme))))
   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind org-roam-capture to C-c n f, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "C-c n f" 'org-roam-capture)
                        scheme))))
   )
  (unless no-maximize
    (emacs-with-nyxt-send-sexps
     '(toggle-fullscreen))))

(defun emacs-with-nyxt-browse-url-nyxt (url &optional buffer-title)
  "Open URL with Nyxt and optionally define BUFFER-TITLE."
  (interactive "sURL: ")
  (emacs-with-nyxt-send-sexps
   (append
    (list
     'buffer-load
     url)
    (if buffer-title
        `(:buffer (make-buffer :title ,buffer-title))
      nil))))

(defun emacs-with-nyxt-close-nyxt-connection ()
  "Close Nyxt connection."
  (interactive)
  (emacs-with-nyxt-send-sexps '(quit)))

(defun browse-url-nyxt (url &optional new-window)
  "Browse URL with Nyxt. NEW-WINDOW is ignored."
  (interactive "sURL: ")
  (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-browse-url-nyxt url url))

(defun emacs-with-nyxt-search-first-in-nyxt-current-buffer (string)
  "Search current Nyxt buffer for STRING."
  (interactive "sString to search: ")
  (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-send-sexps
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
        (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
        (emacs-with-nyxt-send-sexps
         '(ql:quickload "cl-qrencode")
         '(cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png"))
        (find-file "/tmp/qrcode.png")
        (auto-revert-mode))
    (error "You cannot use this until you have Quicklisp installed! Check how to do that at: https://www.quicklisp.org/beta/#installation")))

(defun emacs-with-nyxt-get-nyxt-buffers ()
  "Return nyxt buffers."
  (when (emacs-with-nyxt-connected-p)
    (read
     (emacs-with-nyxt-send-sexps
      '(map 'list (lambda (el) (slot-value el 'title)) (buffer-list))))))

(defun emacs-with-nyxt-nyxt-switch-buffer (&optional title)
  "Interactively switch nyxt buffers.  If argument is provided switch to buffer with TITLE."
  (interactive)
  (if (emacs-with-nyxt-connected-p)
      (let ((title (or title (completing-read "Title: " (emacs-with-nyxt-get-nyxt-buffers)))))
        (emacs-with-nyxt-send-sexps
         `(switch-buffer :id (slot-value (find-if #'(lambda (el) (equal (slot-value el 'title) ,title)) (buffer-list)) 'id))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun emacs-with-nyxt-get-nyxt-commands ()
  "Return nyxt commands."
  (when (emacs-with-nyxt-connected-p)
    (read
     (emacs-with-nyxt-send-sexps
      `(let ((commands (make-instance 'command-source)))
         (map 'list (lambda (el) (slot-value el 'name)) (funcall (slot-value commands 'prompter:CONSTRUCTOR) commands)))))))

(defun emacs-with-nyxt-nyxt-run-command (&optional command)
  "Interactively run nyxt COMMAND."
  (interactive)
  (if (emacs-with-nyxt-connected-p)
      (let ((command (or command (completing-read "Execute command: " (emacs-with-nyxt-get-nyxt-commands)))))
        (emacs-with-nyxt-send-sexps `(nyxt::run-async ',(read command))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun emacs-with-nyxt-nyxt-take-over-prompt ()
  "Take over the nyxt prompt and let Emacs handle completions."
  (interactive)
  (emacs-with-nyxt-send-sexps
   `(progn
      (defun flatten (structure)
        (cond ((null structure) nil)
              ((atom structure) (list structure))
              (t (mapcan #'flatten structure))))
      
      (defun prompt (&REST args)
        (flet ((ensure-sources (specifiers)
                               (mapcar (lambda (source-specifier)
                                         (cond
                                          ((and (symbolp source-specifier)
                                                (c2cl:subclassp source-specifier 'source))
                                           (make-instance source-specifier))
                                          (t source-specifier)))
                                       (uiop:ensure-list specifiers))))
              (sleep 0.1)
              (let* ((promptstring (list (getf args :prompt)))
                     (sources (ensure-sources (getf args :sources)))
                     (names (mapcar (lambda (ol) (slot-value ol 'prompter:attributes)) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (testing (progn
                                (setq my-names names)
                                (setq my-prompt promptstring)))
                     (completed (read-from-string (eval-in-emacs `(emacs-with-nyxt-nyxt-complete ',promptstring ',names))))
                     (suggestion
                      (find-if (lambda (el) (equal completed (slot-value el 'PROMPTER::ATTRIBUTES))) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (selected-class (find-if (lambda (el) (find suggestion (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS))) sources)))
                (if selected-class
                    (funcall (car (slot-value selected-class 'PROMPTER::ACTIONS)) (list (slot-value suggestion 'PROMPTER:VALUE)))
                  (funcall (car (slot-value (car sources) 'PROMPTER::ACTIONS)) (list completed)))))))))

(defun emacs-with-nyxt-nyxt-complete (prompt names)
  "Completion function for nyxt completion."
  (let* ((completions (--map (s-join "\t" (--map (s-join ": " it) it)) names))
         (completed-string (completing-read (s-append ": " (car prompt)) completions))
         (completed-index (-elem-index  completed-string completions)))
    (if (numberp completed-index)
        (nth completed-index names)
      completed-string)))

(provide 'emacs-with-nyxt)
;;; emacs-with-nyxt ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
