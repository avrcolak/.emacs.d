;;; Simple rc to configure go-eldoc.

(y-require 'go-eldoc)

(add-hook 'go-mode-hook 'go-eldoc-setup)
