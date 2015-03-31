;;; A simple rc to customize Emacs' UI to my tastes.

;;; Declutter the UI.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . right)))

(setq inhibit-startup-screen t)

;;; By default Emacs rings the bell on end of buffer.
(setq ring-bell-function 'ignore)

;;; If nil, input (keyboard or mouse) interrupts redisplay.
(setq redisplay-dont-pause t)

;;; Line highlighting.
(global-hl-line-mode t)

;;; By default, only the row number is shown in the modeline.
(column-number-mode t)

;;; Edit visual, not logical lines.
;; (global-visual-line-mode t)

;;; Quick keystroke prompt.
(setq echo-keystrokes 0.0166)

(blink-cursor-mode t)

;;; Paren highlighting on cursor.
(show-paren-mode t)

;;; Use S-<arrow keys> to change windows.
(windmove-default-keybindings)

;;; Richer completion and selection behavior for switching buffers and
;;; finding files.
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(icomplete-mode t)

;;; Likewise for executing commands.
(when (locate-library "smex")
  (autoload 'smex "smex" "Smex stub.")
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)

(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c p") 'proced)
(global-set-key (kbd "C-c f") 'find-function)
(global-set-key (kbd "C-c d") 'yard-toggle-window-dedication)
(global-set-key (kbd "C-x o") 'switch-window)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;;; Less finger travel this way.
(setq switch-window-shortcut-style 'qwerty)

;;; The default escape character is C-c, but C-c is also the usual
;;; control sequence for SIGINT. C-x, by contrast, is just a shell
;;; prefix. Furthermore term-mode "helpfully" treats term-escape-char
;;; as C-x.
(require 'term)
(term-set-escape-char ?\C-x)

;;; Multiple terminal buffers in the same Emacs session.
(ad-enable-advice 'term 'after 'yard-rename-term-buffer)
(ad-activate 'term)

;;; Many programs (including grml zsh) assume this behavior.
(setq term-scroll-to-bottom-on-output 'this)
(setq term-scroll-show-maximum-output t)

;;; Empirically, Emacs on Windows and OS X has a very nice default
;;; font. Likewise, the systems which feature a GTK Emacs are unlikely
;;; to have a very servicable default. However GNU's FreeMono is a
;;; nice Courier clone with good unicode coverage, and is usually
;;; installed.
(when (featurep 'gtk)
  (set-face-attribute 'default nil
                      :family "FreeMono"
                      :height 120
                      :weight 'normal))

;;; Use a nice theme.
;; (load-theme `anti-zenburn t)
;; (load-theme `solarized-dark t)

;;; Make sure, no matter what, ediff uses one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Typing in "yes" and "no" gets tiresome.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; As does being asked for confirmation when opening large files.
(setq large-file-warning-threshold 100000000)

;;; ElDoc uses the minibuffer to prompt parameter lists and docstrings
;;; for Emacs Lisp objects, and also generally (e.g. Clojure's nrepl
;;; uses ElDoc).
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;; Follow compile errors with tab.
(add-hook 'compilation-mode-hook 'next-error-follow-minor-mode)

;;; By default, moving the point past the bottom of a page in docview
;;; does not jump to next page.
(setq doc-view-continuous t)

(setq-default doc-view-resolution 300)

;;; Different sized fonts when editing LaTeX can be disorienting.
(setq font-latex-fontify-sectioning 'color)

;;; Assume Postgres syntax for highlighting SQL.
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
