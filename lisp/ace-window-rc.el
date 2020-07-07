;;; Simple rc to configure ace-window.

(y-pseudo-require 'ace-window)

(global-set-key (kbd "M-o") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-background nil
      aw-dispatch-always t)
