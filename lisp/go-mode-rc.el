;;; Simple rc to configure go-mode.

(when (yard-require 'go-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "M-.") 'godef-jump))
