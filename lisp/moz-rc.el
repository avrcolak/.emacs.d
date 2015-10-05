;;; Simple rc to load and configure moz.

(y-package-install 'moz)

(autoload 'moz-minor-mode "moz" "Inferior and minor modes for MozRepl." t)
(add-hook 'javascript-mode-hook 'moz-minor-mode)
