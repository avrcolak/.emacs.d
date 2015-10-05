;;; Simple rc to configure ace-window.

(y-require 'ace-window)

(global-set-key (kbd "C-x o") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-background nil
      aw-dispatch-always t)
