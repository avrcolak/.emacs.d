;;; Simple rc to load and configure MozRepl's Emacs functionality.

;;; moz.el is not (yet) in any of the usual package repositories, so
;;; check if it's on the file system first.
(let ((mozdule (expand-file-name "~/.emacs.d/non-elpa/moz.el")))
  (when (file-exists-p mozdule)
    (autoload
      `moz-minor-mode mozdule
      "Inferior and minor modes for MozRepl."
      t)
    ;; ECMAScript has too many use cases to warrant a default REPL.
    ;; (add-hook 'javascript-mode-hook 'moz-minor-mode)
    ))
