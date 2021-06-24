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


(defvar emacs-with-nyxt-slime-nyxt-delay
  0.3
  "Delay to wait for Slime commands to reach Nyxt.")


(defun emacs-with-nyxt-slime-connect (host port)
  "Connect Slime to HOST and PORT ignoring version mismatches."
  (defun true (&rest args) 't)
  (advice-add 'slime-check-version :override #'true)
  (slime-connect host port)
  (sleep-for emacs-with-nyxt-slime-nyxt-delay)
  (advice-remove 'slime-check-version #'true))

(defun emacs-with-nyxt-slime-repl-send-sexps (&rest s-exps)
  "Evaluate S-EXPS with Nyxt Slime session."
  (let ((s-exps-string (s-join "" (--map (prin1-to-string it) s-exps))))
    (defun true (&rest args) 't)
    (advice-add 'slime-check-version :override #'true)
    (if (slime-connected-p)
        (slime-repl-send-string s-exps-string)
      (error "Slime is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first"))
    (sleep-for emacs-with-nyxt-slime-nyxt-delay)
    (advice-remove 'slime-check-version #'true)))

(defun emacs-with-nyxt-start-and-connect-to-nyxt (&optional no-maximize)
  "Start Nyxt with swank capabilities. Optionally skip window maximization with NO-MAXIMIZE."
  (interactive)
  (async-shell-command (format "nyxt -e \"(nyxt-user::start-swank)\""))
  (sleep-for emacs-with-nyxt-slime-nyxt-delay)
  (emacs-with-nyxt-slime-connect "localhost" "4006")
  (emacs-with-nyxt-slime-repl-send-sexps
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


;;; emacs-with-nyxt ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
