;;; A simple rc to configure switch-window.

(y-require 'switch-window)

(global-set-key (kbd "C-x o") 'switch-window)

;; Less finger travel this way.
(setq switch-window-shortcut-style 'qwerty)
