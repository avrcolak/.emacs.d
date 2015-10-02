;;; Simple rc to configure go-eldoc.

(when (yard-require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup))
