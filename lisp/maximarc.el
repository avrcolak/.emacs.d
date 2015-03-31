;;; An rc to setup Emacs as a Maxima client.

;;; I can't without Common Lisp.
(require 'cl-lib)

(defvar maxima-path '("~/share/maxima"
                      "/usr/local/share/maxima"
                      "/usr/share/maxima")
  "List of directories to search for a distribution of Maxima.")

(defun find-maxima-emacs-contrib ()
  (cl-find-if (lambda (path) (file-exists-p path))
              (cl-mapcan (lambda (path)
                           (mapcar (lambda (path)
                                     (concat (file-name-as-directory path)
                                             "emacs/")) 
                                   ;; Prefer larger version numbers.
                                   (sort (file-expand-wildcards
                                          (concat (file-name-as-directory
                                                   path)
                                                  "*"))
                                         #'string-greaterp)))
                         maxima-path)))

(defun setup-maxima ()
  ;; Find the emacs directory.
  (let ((maxima-emacs-contrib (find-maxima-emacs-contrib)))
    (when maxima-emacs-contrib
      ;; Use maxima.el.
      (add-to-list 'load-path maxima-emacs-contrib)
      (autoload 'maxima-mode "maxima" "Maxima mode." t)
      (autoload 'maxima "maxima" "Maxima interaction." t)
      (add-to-list 'auto-mode-alist '("\\.ma?[cx]$" . maxima-mode))
      ;; Also use imaxima.el.
      (autoload 'imaxima "imaxima" "Maxima interaction with images." t)
      (setq imaxima-fnt-size "Large"
            imaxima-use-maxima-mode-flag t))))

(setup-maxima)
