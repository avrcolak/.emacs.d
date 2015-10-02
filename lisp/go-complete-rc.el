;;; Simple rc to configure go-complete.

(when (yard-require 'go-complete)
  ;; According to the documentation of
  ;; `completion-at-point-functions', `add-hook' is the appropriate
  ;; verb here.
  (add-hook 'completion-at-point-functions 'go-complete-at-point))
