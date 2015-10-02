;;; Simple rc to configure term. Simplified by preferring ansi-term
;;; defaults.

(when (yard-require 'term)
  ;; Many programs (including grml zsh) (seem to) assume this
  ;; behavior.
  (setq term-scroll-to-bottom-on-output 'this)
  (setq term-scroll-show-maximum-output t))
