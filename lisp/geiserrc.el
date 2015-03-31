;;; Simple rc to configure Geiser

(setq geiser-active-implementations '(racket))

;;; A bit more leadway on slower machines.
(setq geiser-repl-startup-time 10000)

;;; Use chronological history.
(setq geiser-repl-history-no-dups-p nil)

(add-hook 'geiser-repl-mode-hook 'paredit-mode)
