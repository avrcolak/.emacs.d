;;; Simple rc to configure gambit.

(when (yard-optimistically-locate-feature 'gambit)
  ;; Following (perhaps uncritically) section 5.6 of the Gambit-C
  ;; manual.
  (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
  (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
  (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
  (add-hook 'scheme-mode-hook 'gambit-mode)
  (setq scheme-program-name "gambitc -:d-"))
