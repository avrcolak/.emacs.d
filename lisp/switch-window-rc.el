;;; A simple rc to configure switch-window.

(when (yard-require 'switch-window)
  (global-set-key (kbd "C-x o") 'switch-window)
  ;; Less finger travel this way.
  (setq switch-window-shortcut-style 'qwerty))
