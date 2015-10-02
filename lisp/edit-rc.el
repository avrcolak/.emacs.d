;;; A simple rc to configure Emacs' editing behavior.

;;; Spaces instead of tabs.
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;; Turn on some "confusing" commands.
(put `upcase-region `disabled nil)
(put `downcase-region `disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; "stroustrup" and "k&r" in lieu of "gnu".
(setq-default c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (c++-mode . "stroustrup")
                                (other . "k&r")))

;;; Java is conventionally camel cased.
(add-hook 'java-mode-hook 'subword-mode)

;;; Use buffer local 'lisp-indent-function values corresponding to the
;;; mode.
(add-hook 'lisp-mode-hook 'yard-set-common-lisp-indent)
(add-hook 'inferior-lisp-mode-hook 'yard-set-common-lisp-indent)
(add-hook 'lisp-interaction-mode-hook 'yard-set-elisp-indent)
(add-hook 'ielm-mode-hook 'yard-set-elisp-indent)
(add-hook 'emacs-lisp-mode-hook 'yard-set-elisp-indent)

;;; Automatic minor modes.
(add-hook 'find-file-hook 'yard-set-auto-minor-mode)

;;; Sane indentation of the loop macro in Common Lisp.
(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)

;;; .paren is the usual extension for Parenscript files.
(add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
