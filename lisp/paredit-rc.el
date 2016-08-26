;;; Simple rc to hook paredit onto various s-expression language
;;; modes.

(y-pseudo-require 'paredit)

(y-add-hooks '(emacs-lisp-mode-hook
               ielm-mode-hook
               inferior-lisp-mode-hook
               inferior-scheme-mode-hook
               lisp-interaction-mode-hook
               lisp-mode-hook
               scheme-mode-hook
               slime-inferior-process-start-hook
               slime-mode-hook
               slime-repl-mode-hook)
             'paredit-mode)
