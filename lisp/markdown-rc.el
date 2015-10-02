;;; Simple rc to configure markdown-mode.

(when (yard-require 'markdown-mode)
  ;; markdown-mode fontifies buffers even if `font-lock-mode' is nil
  ;; because it calls `font-lock-refresh-defaults'. However the caller
  ;; does nothing important, so clobber it. There is a fix for this
  ;; floating around upstream.
  (fset 'markdown-reload-extensions #'yard-do-nothing)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
