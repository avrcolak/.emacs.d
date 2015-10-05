;;; Simple rc to configure wolfram-mode.

(y-package-install 'wolfram-mode)

(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
