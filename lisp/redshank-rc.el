;;; A simple rc to hook redshank onto various modes for editing Common
;;; Lisp.

(when (yard-require 'redshank)
  (yard-add-hooks '(inferior-lisp-mode-hook
                    lisp-mode-hook
                    slime-inferior-process-start-hook
                    slime-mode-hook
                    slime-repl-mode-hook)
                  'redshank-mode))
