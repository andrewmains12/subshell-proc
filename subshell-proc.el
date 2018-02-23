;;; subshell-proc.el --- Functions for working with comints
;; Author: Andrew Mains
;; URL: https://github.com/andrewmains12/subshell-proc
;; Version: 0.3
;;
;; License: GPL-3+
;;
;; Copyright (c) 2012 Andrew Mains
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Sample usage:
;;
;; ;;In elisp code
;; (defproc noir-server "lein" '("run")
;; "Run a noir server")
;;
;; While editing:
;;   M-x run-proc
;;     bash
;;   will bring up a comint running bash
;;
;;

(require 'cl)


(defmacro defproc (fn-name command command-args &optional docstring)
  "Defines an interactive function which creates a comint subprocess using command"
    `(defun ,fn-name (&rest extra-args)
       ,docstring
       (interactive)
       (funcall
        (make-proc-run-fn ,command ,command-args
                          ,(format "*%s*" (symbol-name fn-name))))
     ))


(defun make-proc-run-fn (command command-args &optional buffer-name)
  (lexical-let ((buffer-name buffer-name)
                (command command)
                (command-args command-args)
                )
    (function (lambda (&rest extra-args)
                (let* ((buffer-name (or buffer-name (format "*%s*" command)))
                       (buffer (get-buffer-create buffer-name))
                       )
                  (pop-to-buffer buffer)
                  (apply 'make-comint-in-buffer
                         (append (list buffer-name buffer command nil) command-args))
                  )))))


(defun run-proc (command &optional buffer-name)
  (interactive "MCommand to run: ")
  (let ((command-list (split-string command)))
    (funcall (make-proc-run-fn (car command-list) (cdr command-list) buffer-name))))

(provide 'subshell-proc)


;;; subshell-proc.el ends here
