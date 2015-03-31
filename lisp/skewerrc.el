;;; A simple rc to set up Skewer and simple-httpd.

(when (require 'skewer nil t)
  ;; 1030 is a nice, not yet IANA assigned port number.
  (setq httpd-port 1030)
  ;; Hook into js2-mode, css-mode and html-mode.
  (skewer-setup))
