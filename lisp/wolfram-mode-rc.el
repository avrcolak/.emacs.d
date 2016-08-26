;;; Simple rc to configure wolfram-mode.

(y-pseudo-require 'wolfram-mode)

(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
