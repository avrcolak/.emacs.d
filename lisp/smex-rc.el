;;; Simple rc to configure smex.

(y-require 'smex)

;; HACK: smex version 3.0 requires a newer `execute-extended-command',
;; but loads even if it is not present (newer versions signal an
;; error, in which case control flow will never reach this point). On
;; the other hand, older versions of smex are happy with an older
;; `execute-extended-command'. Since smex does not (has not
;; historically) provid(ed) a means to get the version, one
;; (inelegant) way to both always clobber M-x with a working `smex'
;; and avoid clobbering with a non-working `smex' is to test `smex'
;; directly.
(unless (condition-case nil
            (y-flet
             ;; `smex' attempts to execute whatever
             ;; `smex-completing-read' returns.
             ((smex-completing-read (choices &optional initial-selection)
                                    (symbol-name 'y-do-nothing)))
             (smex))
          (error t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))
