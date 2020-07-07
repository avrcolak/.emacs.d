;;; A simple rc to configure Emacs' editing behavior.

(delete-selection-mode)

;; "stroustrup" and "k&r" in lieu of "gnu".
(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (c++-mode . "stroustrup")
                                (other . "k&r")))

;; Java is conventionally camel cased.
(add-hook 'java-mode-hook 'subword-mode)

(defun y-set-lisp-indent (indent-function)
  "Shadows `lisp-indent-function' with a buffer local variable
set to INDENT-FUNCTION."
  (set (make-local-variable 'lisp-indent-function) indent-function))

(defun y-set-elisp-indent ()
  (y-set-lisp-indent 'lisp-indent-function))

(defun y-set-common-lisp-indent ()
  (y-set-lisp-indent 'common-lisp-indent-function))

;; Use buffer local 'lisp-indent-function values corresponding to the
;; mode.
(add-hook 'lisp-mode-hook 'y-set-common-lisp-indent)
(add-hook 'inferior-lisp-mode-hook 'y-set-common-lisp-indent)
(add-hook 'lisp-interaction-mode-hook 'y-set-elisp-indent)
(add-hook 'ielm-mode-hook 'y-set-elisp-indent)
(add-hook 'emacs-lisp-mode-hook 'y-set-elisp-indent)

(add-hook 'find-file-hook 'y-set-auto-minor-mode)

;; Sane indentation of the loop macro in Common Lisp.
(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)

;; .paren is the usual extension for Parenscript files.
(add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
