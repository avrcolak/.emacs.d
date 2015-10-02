;;; A simple rc to configure ido.

(when (yard-require 'ido)
  (ido-mode 1)
  (ido-everywhere))
