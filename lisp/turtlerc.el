;;; Simple rc to configure turtle mode.

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files." t)

(setq auto-mode-alist (append (list '("\\.n3" . ttl-mode)
                                    '("\\.ttl" . ttl-mode))
                              auto-mode-alist))

