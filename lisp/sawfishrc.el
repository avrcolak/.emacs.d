;;; A simple rc to configure Sawfish mode.

(add-hook 'sawfish-mode-hook 'paredit-mode)
(add-to-list 'auto-mode-alist '(".*sawfishrc\\'" . sawfish-mode ))
(add-to-list 'auto-mode-alist '(".*\\.jl\\'" . sawfish-mode ))
(add-to-list 'ecb-compilation-buffer-names '("*sawfish*"))
