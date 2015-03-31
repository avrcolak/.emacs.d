;;; A simple rc to load some useful packages in a (hopefully)
;;; repeatable way.

(let ((default-directory "~/.emacs.d/elpa"))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; For Emacs Lisp not managed by Emacs' package system.
(add-to-list 'load-path "~/.emacs.d/non-elpa")

(when (require `package nil t)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (let ((required-packages '(adaptive-wrap
                             auctex
                             cider
                             clojure-mode
                             graphviz-dot-mode
                             geiser
                             gnuplot
                             haskell-mode
                             js2-mode
                             lua-mode
                             paredit
                             php-mode
                             redshank
                             smex
                             switch-window)))
    (dolist (p required-packages)
      (when (not (package-installed-p p))
        (package-install p)))))
