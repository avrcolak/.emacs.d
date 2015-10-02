;;; A simple rc to customize Emacs' UI to my tastes.

;;; Declutter the GUI.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)))

(setq inhibit-startup-screen t)

;;; By default Emacs rings the bell on end of buffer, and other
;;; unecessary times.
(setq ring-bell-function 'ignore)

;;; If nil, input (keyboard or mouse) interrupts redisplay.
(setq redisplay-dont-pause t)

;;; By default, only the row number is shown in the modeline.
(column-number-mode)

;;; The Commander Pike school of thought on syntax highlighting.
(global-font-lock-mode -1)

;;; Quick keystroke prompt.
(setq echo-keystrokes 0.0166)

;;; Aristotelean scrolling maps better to the hardware I use.
(setq mouse-wheel-progressive-speed nil)

;;; Highlight matching parenthesis at point.
(show-paren-mode)

;;; Use S-<arrow keys> to change windows.
(windmove-default-keybindings)

;;; Minibuffer tab completion.
(icomplete-mode)

(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)

(global-set-key (kbd "C-c p") 'proced)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c d") 'yard-toggle-window-dedication)
(global-set-key (kbd "C-c q") 'yard-quit-other-window)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

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

;;; Make sure, no matter what, ediff uses one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; Going over this threshold results in an annoying warning, so make
;;; it high.
(setq large-file-warning-threshold 100000000)

;;; ElDoc uses the minibuffer to prompt parameter lists and docstrings
;;; for Emacs Lisp objects, and also generally (e.g. Clojure's nrepl
;;; uses ElDoc).
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; Follow compile errors with tab.
(add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)

;;; By default, moving the point past the bottom of a page in docview
;;; does not jump to next page.
(setq doc-view-continuous t)

(setq-default doc-view-resolution 300)

;;; Enable some "confusing" commands.
(put 'set-goal-column 'disabled nil)
(put 'erase-buffer 'disabled nil)
