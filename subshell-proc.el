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
;;
;; While editing:
;;   M-x run-proc
;;     bash
;;   will bring up a comint running bash
;;
;;

(defmacro defproc (fn-name command &optional command-args docstring)
  "Defines an interactive function which creates a comint subprocess using command"
    `(defun ,fn-name (&rest extra-args)
       ,docstring
       (interactive)
       (funcall
        (make-proc-run-fn ,command ,command-args
                          ,(format "*%s*" (symbol-name fn-name))))))

(defun make-proc-run-fn (command &optional command-args buffer-name)
  (lambda (&rest extra-args)
    (let* ((buffer-name (or buffer-name (format "*%s*" command)))
           (buffer (get-buffer-create buffer-name)))
      (pop-to-buffer buffer)
      (apply 'make-comint-in-buffer
             (append (list buffer-name buffer command nil) command-args)))))

(defun run-proc (command &optional buffer-name)
  (interactive "MCommand to run: ")
  (let ((command-list (split-string command)))
    (funcall (make-proc-run-fn (car command-list) (cdr command-list) buffer-name))))

(provide 'subshell-proc)


;;; subshell-proc.el ends here
