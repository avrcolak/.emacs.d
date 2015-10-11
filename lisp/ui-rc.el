;;; A simple rc to customize Emacs' UI to my tastes.

;; Declutter the GUI.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)))

(setq inhibit-startup-screen t)

;; Empirically, Emacs on Windows and OS X has a very nice default
;; font. On the other hand, the systems which feature a GTK Emacs are
;; unlikely to have a very servicable default. GNU's FreeMono is a
;; nice Courier clone with good unicode coverage, and is usually
;; installed.
(when (featurep 'gtk)
  (set-face-attribute 'default nil
                      :family "FreeMono"
                      :height 120
                      :weight 'normal))

;; If nil, input (keyboard or mouse) interrupts redisplay.
(setq redisplay-dont-pause t)

;; Font lock is Emacs for "syntax highlighting".
(global-font-lock-mode -1)

;; By default, only the row number is shown in the modeline.
(column-number-mode)

;; Highlight matching parenthesis at point.
(show-paren-mode)

(setq-default doc-view-resolution 300)

;; eldoc-mode employs the minibuffer to prompt parameter lists and
;; docstrings for Emacs Lisp objects, and also generally
;; (e.g. Clojure's nrepl uses eldoc).
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Prompt keystrokes quickly.
(setq echo-keystrokes 0.0166)

;; By default, Emacs rings the bell on end of buffer, and other
;; unecessary times.
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

;; The default assumes far less capable hardware than I usually
;; operate.
(setq large-file-warning-threshold 100000000)

;; Make sure, no matter what, ediff uses one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq even-window-heights nil)

;; Minibuffer tab completion.
(icomplete-mode)

(when (require 'ido nil t)
  (ido-mode 1)
  (ido-everywhere))

;; Slightly simplified by preferring ansi-term defaults.
(eval-after-load 'term
  ;; Many programs (including grml zsh) (seem to) assume this
  ;; behavior.
  '(setq term-scroll-to-bottom-on-output 'this
         term-scroll-show-maximum-output t))

;; Follow compile errors with tab.
(add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)

;; By default, moving the point past the bottom of a page in docview
;; does not jump to next page.
(setq doc-view-continuous t)

(setq ispell-dictionary "en_GB")

;; Enable some "confusing" commands.
(put 'set-goal-column 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put `upcase-region `disabled nil)
(put `downcase-region `disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; The defaults in this case are S-<arrow keys>.
(windmove-default-keybindings)

(global-set-key (kbd "C-c p") 'proced)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c d") 'y-toggle-window-dedication)
(global-set-key (kbd "C-c q") 'y-quit-other-window)
(global-set-key (kbd "C-c r") 'y-root-find-file)
(global-set-key (kbd "C-c c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-h 9") 'y-slovnik-lookup)

(define-key emacs-lisp-mode-map (kbd "M-.")
  'y-find-function-at-point)

(define-key emacs-lisp-mode-map (kbd "M-*")
  'y-pop-find-symbol-stack)

(setq mouse-wheel-progressive-speed nil ; Aristotelean scrolling
      focus-follows-mouse t
      mouse-autoselect-window t
      mouse-yank-at-point t)

(define-key emacs-lisp-mode-map (kbd "<C-S-mouse-1>")
  (y-with-point-at-click 'y-find-function-at-point))

(define-key emacs-lisp-mode-map (kbd "<mouse-8>")
  'y-pop-find-symbol-stack)

(eval-after-load 'help-mode
  '(progn
     (define-key help-mode-map (kbd "<mouse-8>") 'help-go-back)
     (define-key help-mode-map (kbd "<mouse-9>") 'help-go-forward)))

(eval-after-load 'Info-mode
  '(progn
     (define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
     (define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)))
