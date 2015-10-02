;; Simple rc to configure wolfram-mode. wolfram-mode is an Emacs
;; interface to the Mathematica kernel and language.

(when (yard-require 'wolfram-mode)
  (add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode)))
