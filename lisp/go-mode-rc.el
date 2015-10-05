;;; Simple rc to configure go-mode.

(y-require 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
