;;; A simple rc to configure Emacs' editing behavior.

;;; Spaces instead of tabs.
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;; Rectangular selection.
;;; (cua-selection-mode t)

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

;;; Paredit for all the s-expression language modes.
(when (require 'paredit nil t)
  (dolist (mode-hook '(clojure-mode-hook
                       emacs-lisp-mode-hook
                       inferior-lisp-mode-hook
                       lisp-interaction-mode-hook
                       ielm-mode-hook
                       lisp-mode-hook
                       cider-repl-mode-hook
                       inferior-scheme-mode-hook
                       scheme-mode-hook))
    (add-hook mode-hook 'paredit-mode)))

;;; Redshank refactoring for Common Lisp.
(when (require 'redshank nil t)
  (add-hook 'lisp-mode-hook 'redshank-mode)
  (add-hook 'inferior-lisp-mode-hook 'redshank-mode))

;;; Use buffer local 'lisp-indent-function values corresponding to the
;;; mode.
(add-hook 'lisp-mode-hook 'yard-set-common-lisp-indent)
(add-hook 'inferior-lisp-mode-hook 'yard-set-common-lisp-indent)
(add-hook 'lisp-interaction-mode-hook 'yard-set-elisp-indent)
(add-hook 'ielm-mode-hook 'yard-set-elisp-indent)
(add-hook 'emacs-lisp-mode-hook 'yard-set-elisp-indent)

;;; Clojure is married to Java which is married to camel case.
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

;;; Clobber javascript-mode with js2-mode.
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; Automatic minor modes.
(add-hook 'find-file-hook 'yard-set-auto-minor-mode)

;;; Sane indentation of the loop macro in Common Lisp.
(setq lisp-simple-loop-indentation 2
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 9)
