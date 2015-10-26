;;; subshell-proc.el --- Functions for working with comints -*- lexical-binding: -*-
;; Author: Andrew Mains
;; URL: https://github.com/andrewmains12/subshell-proc
;; Version: 0.2
;;
;; Sample usage:
;;
;; ;;In elisp code
;; (defproc noir-server "lein" '("run")
;; "Run a noir server")
;; (defproc echo-test "echo" `(,buffer-file-name)
;; "Print `buffer-file-name'")
;; (defproc echo-test2 "echo" `(,buffer-file-name)
;; "Print `buffer-file-name' without selecting *echo* buffer."
;; 'display-buffer)
;; (defproc xeyes "xeyes" nil
;; "Run xeyes without popping up *xeyes* buffer"
;; 'ignore)
;;
;; While editing:
;;   M-x run-proc
;;     bash
;;   will bring up a comint running bash
;;
;;

(defmacro defproc (fn-name command &optional command-args docstring display-fn)
  "Defines an interactive function which creates a comint subprocess using command"
    `(defun ,fn-name (&rest extra-args)
       ,docstring
       (interactive)
       (funcall
        (make-proc-run-fn ,command ,command-args
                          ,(format "*%s*" (symbol-name fn-name))
                          ,display-fn))))

(defun make-proc-run-fn (command &optional command-args buffer-name display-fn)
  (lambda (&rest extra-args)
    (let* ((buffer-name (or buffer-name (format "*%s*" command)))
           (buffer (get-buffer-create buffer-name)))
      (funcall (or display-fn 'pop-to-buffer) buffer)
      (apply 'make-comint-in-buffer
             (append (list buffer-name buffer command nil) command-args)))))

(defun run-proc (command &optional buffer-name)
  (interactive "MCommand to run: ")
  (funcall (make-proc-run-fn shell-file-name
                             (list shell-command-switch command)
                             buffer-name)))

(provide 'subshell-proc)


;;; subshell-proc.el ends here
