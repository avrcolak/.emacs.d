;;; Simple rc to configure markdown-mode.

(y-require 'markdown-mode)

;; HACK: markdown-mode fontifies buffers even if `font-lock-mode' is
;; nil, because it calls `font-lock-refresh-defaults'. However the
;; caller does nothing important, so clobber it. There is a fix for
;; this floating around upstream.
(fset 'markdown-reload-extensions #'y-do-nothing)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
