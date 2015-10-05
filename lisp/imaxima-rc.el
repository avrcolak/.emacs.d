;;; An rc to setup imaxima (and maxima).

;; On some systems, the Maxima Emacs contribs aren't installed in any
;; of the usual load path directories. So, attempt to add wherever
;; they are to the load path.

(defvar y-maxima-paths '("~/share/maxima"
                           "/usr/local/share/maxima"
                           "/usr/share/maxima")
  "List of directories to search for a distribution of Maxima.")

(defun y-find-latest-maxima (paths)
  (car (sort (apply #'append
                    (mapcar (lambda (path)
                              (file-expand-wildcards
                               (concat (file-name-as-directory path)
                                       "*")))
                            paths))
             #'string<)))

(defun y-find-maxima-emacs-contrib (paths)
  (let ((path (concat (file-name-as-directory (y-find-latest-maxima paths))
                      "emacs/")))
    (when (file-exists-p path) path)))

(let ((contrib (y-find-maxima-emacs-contrib y-maxima-paths)))
  (when contrib
    (add-to-list 'load-path contrib)))

(autoload 'maxima-mode "maxima" "Major mode for editing Maxima mode." t)
(autoload 'maxima "maxima" "Run Maxima interactively." t)
(autoload 'imaxima "imaxima" "Image support for Maxima" t)

(add-to-list 'auto-mode-alist '("\\.ma?[cx]$" . maxima-mode))
(setq imaxima-fnt-size "Large"
      imaxima-use-maxima-mode-flag t)
