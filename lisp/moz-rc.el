;;; Simple rc to load and configure moz.

(when (yard-optimistically-locate-feature 'moz)
  (autoload `moz-minor-mode "moz" "Inferior and minor modes for MozRepl." t)
  ;; ECMAScript has too many use cases to warrant a default REPL.
  ;; (add-hook 'javascript-mode-hook 'moz-minor-mode)
  )
