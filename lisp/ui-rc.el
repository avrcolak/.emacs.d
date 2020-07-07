;;; A simple rc to customize Emacs' UI to my tastes.

;; Declutter the GUI.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (background-color . "honeydew")
        (vertical-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)))

(setq inhibit-startup-screen t)

;; If nil, input (keyboard or mouse) interrupts redisplay.
(setq redisplay-dont-pause t)

;; By default, only the row number is shown in the modeline.
(column-number-mode)

;; Highlight matching parenthesis at point.
(show-paren-mode)

(setq-default doc-view-resolution 300)

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

(eval-after-load 'term
  ;; Many programs (including grml zsh) (seem to) assume this
  ;; behavior.
  '(progn (setq term-scroll-to-bottom-on-output 'this
                term-scroll-show-maximum-output t)
          ;; This is the ansi-term default, but not serial-term.
          (term-set-escape-char ?\C-x)))

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

(setq mouse-wheel-progressive-speed nil ; Aristotelean scrolling
      focus-follows-mouse t
      mouse-autoselect-window nil
      mouse-yank-at-point t)

(eval-after-load 'help-mode
  '(progn
     (define-key help-mode-map (kbd "<mouse-5>") 'help-go-back)
     (define-key help-mode-map (kbd "<mouse-4>") 'help-go-forward)))

(eval-after-load 'Info-mode
  '(progn
     (define-key Info-mode-map (kbd "<mouse-5>") 'Info-history-back)
     (define-key Info-mode-map (kbd "<mouse-4>") 'Info-history-forward)))
