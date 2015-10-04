;;; A simple rc to customize Emacs' UI to my tastes.

;;; Declutter the GUI.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)))

(setq inhibit-startup-screen t)

;;; Empirically, Emacs on Windows and OS X has a very nice default
;;; font. On the other hand, the systems which feature a GTK Emacs are
;;; unlikely to have a very servicable default. GNU's FreeMono is a
;;; nice Courier clone with good unicode coverage, and is usually
;;; installed.
(when (featurep 'gtk)
  (set-face-attribute 'default nil
                      :family "FreeMono"
                      :height 120
                      :weight 'normal))

;;; If nil, input (keyboard or mouse) interrupts redisplay.
(setq redisplay-dont-pause t)

;;; Font lock is Emacs for "syntax highlighting".
(global-font-lock-mode -1)

;;; By default, only the row number is shown in the modeline.
(column-number-mode)

;;; Highlight matching parenthesis at point.
(show-paren-mode)

(setq-default doc-view-resolution 300)

;;; eldoc-mode employs the minibuffer to prompt parameter lists and
;;; docstrings for Emacs Lisp objects, and also generally
;;; (e.g. Clojure's nrepl uses eldoc).
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; Prompt keystrokes quickly.
(setq echo-keystrokes 0.0166)

;;; By default, Emacs rings the bell on end of buffer, and other
;;; unecessary times.
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; The default assumes far less capable hardware than I usually
;;; operate.
(setq large-file-warning-threshold 100000000)

;;; Make sure, no matter what, ediff uses one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Minibuffer tab completion.
(icomplete-mode)

;;; Follow compile errors with tab.
(add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)

;;; By default, moving the point past the bottom of a page in docview
;;; does not jump to next page.
(setq doc-view-continuous t)

;;; Enable some "confusing" commands.
(put 'set-goal-column 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put `upcase-region `disabled nil)
(put `downcase-region `disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; The defaults in this case are S-<arrow keys>.
(windmove-default-keybindings)

(global-set-key (kbd "C-c p") 'proced)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c d") 'yard-toggle-window-dedication)
(global-set-key (kbd "C-c q") 'yard-quit-other-window)
(global-set-key (kbd "C-c r") 'yard-root-find-file)
(global-set-key (kbd "C-c c") 'save-buffers-kill-emacs)

(define-key emacs-lisp-mode-map (kbd "M-.")
  'yard-find-function-at-point)

(define-key emacs-lisp-mode-map (kbd "M-*")
  'yard-pop-find-symbol-stack)

;;; Aristotelean scrolling maps better to the hardware I use.
(setq mouse-wheel-progressive-speed nil)

(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)

(define-key emacs-lisp-mode-map (kbd "<C-S-mouse-1>")
  (lambda (event prefix-arg)
    (interactive "@e
P")
    (mouse-set-point event)
    (yard-find-function-at-point prefix-arg)))

(define-key emacs-lisp-mode-map (kbd "<mouse-9>")
  'yard-pop-find-symbol-stack)

(define-key help-mode-map (kbd "<mouse-8>") 'help-go-forward)
(define-key help-mode-map (kbd "<mouse-9>") 'help-go-back)
