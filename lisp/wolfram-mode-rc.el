;; Simple rc to configure wolfram-mode.

(when (yard-require 'wolfram-mode)
  (add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode)))
