;;; The Emacs Lisp YARD: Yet Another Run command Directory. Some
;;; useful Emacs Lisp functions for my Emacs rc files. Any
;;; similarities to snippets of elisp found around the web may or may
;;; not be accidental.

(defun yard-directory-sub-directories (dirs) 
  "Returns a list of the subdirectories for each directory in dirs."
  (delq nil
        (mapcar (lambda (x) (and (file-directory-p x) x))
                (apply 'append
                       (let (sub-dirs)
                         (dolist (dir dirs sub-dirs)
                           (setq sub-dirs
                                 (cons (directory-files dir t "[[:word:]]+")
                                       sub-dirs))))))))

;;; Helpers for setting up Lisp indentation.
(defun yard-set-lisp-indent (indent-function)
  (set (make-local-variable 'lisp-indent-function) indent-function))

(defun yard-set-elisp-indent ()
  (yard-set-lisp-indent 'lisp-indent-function))

(defun yard-set-common-lisp-indent ()
  (yard-set-lisp-indent 'common-lisp-indent-function))

;;; Functions for opening a file as root with tramp.
(defvar yard-root-find-file-prefix "/sudo:root@localhost:"
  "The prefix used to open a file with `yard-root-find-file'.")

(defvar yard-root-find-file-history nil
  "A list holding files previously opened with `yard-root-find-file'.")

(defvar yard-root-find-file-hook nil
  "A hook for functions to run after a file has been opened with
  `yard-root-find-file'.")

(defun yard-root-find-file ()
  "Open a file as the root user. Prepends
  `yard-root-find-file-prefix' to the selected file name for
  access with tramp."
  (interactive)
  (require 'tramp)
  (let* ((file-name-history yard-root-find-file-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)
    (when tramp
      (setq path (tramp-file-name-localname tramp)
            dir (file-name-directory path)))
    (when (setq file (read-file-name "Find file (root): " dir path))
      (find-file (concat yard-root-find-file-prefix file))
      (setq yard-root-find-file-history file-name-history)
      (run-hooks yard-root-find-file-hook))))

;;; Toggle window dedication to lock or pin a window.
(defun yard-toggle-window-dedication ()
  "Toggles a window from dedicated to not dedicated. See Info
node `Dedicated Windows'."
  (interactive)
  (let ((window (selected-window)))
    (set-window-dedicated-p window
                            (not (window-dedicated-p window)))
    (message (if (window-dedicated-p window)
                 "Window %s dedicated"
               "Window %s not dedicated")
             window)))

(defun yard-slime-send-dwim (arg)
  "Send the code form you want to SLIME (Do What I Mean)
If the region is active it is copied to the SLIME REPL.
Else, if the point is at an opening paren the sexp immediately
following the point is copied to the SLIME REPL.
Else, if the point directly after a closing paren, the sexp
immediately preceding the point is copied to the SLIME REPL.
Else, the top level sexp enclosing the point is copied to the
SLIME REPL."
  (interactive "P")
  (save-excursion
    (cond (mark-active
           (copy-region-as-kill (mark) (point)))
          ((eq (char-after) ?\()
           (let ((beg (point))
                 (end (save-excursion (forward-sexp) (point))))
             (copy-region-as-kill beg end)))
          ((eq (char-before) ?\))
           (let ((end (point))
                 (beg (save-excursion (backward-sexp) (point))))
             (copy-region-as-kill beg end)))
          (t
           (let* ((beg (progn (beginning-of-defun)
                              (point)))
                 (end (save-excursion (end-of-defun) (point))))
             (copy-region-as-kill beg end))))
    (save-window-excursion
      (switch-to-buffer (slime-output-buffer))
      (goto-char (point-max))
      (when (string-match "\n\\|" (car kill-ring))
        (slime-repl-newline-and-indent))
      (yank)
      (when arg
        (slime-repl-return)))))

(defun yard-enclose-region-in-src-block ()
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (if (region-active-p) (region-end) (point))))
    (goto-char end)
    (unless (eq (char-before) ?\n) (insert "\n"))
    (insert "#+END_SRC\n")
    (goto-char beg)
    (beginning-of-line)
    (insert "#+BEGIN_SRC\n")
    (backward-char)))

;;; Automatic minor modes.
(defvar yard-auto-minor-mode-alist ()
  "Alist of file name patterns vs corresponding minor mode
functions. Closely mimics `auto-mode-alist'.")

(defun yard-set-auto-minor-mode ()
  "Select minor modes appropriate for curent buffer.

To find the right minor modes, this function compares the
filename against all entries in `yard-auto-minor-mode-alist' and
enables the specified minor modes."
  (when buffer-file-name
    (let ((remote-id (file-remote-p buffer-file-name))
          (name buffer-file-name))
      ;; Clean up the file name for this buffer.
      (setq name (file-name-sans-versions name))
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (dolist (entry yard-auto-minor-mode-alist)
        (when (and (car entry) (cdr entry))
          (if (string-match (car entry) name)
              (funcall (cdr entry))))))))

(defvar yard-super-meta-mode-syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\@ "'" table)
    (modify-syntax-entry ?\$ "'" table)
    (modify-syntax-entry ?\! "'" table)
    (modify-syntax-entry ?\% "'" table)
    (modify-syntax-entry ?\? "'" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table used in `yard-super-meta-mode'.")

(define-derived-mode yard-super-meta-mode lisp-mode
  "Super Meta"
  "Major mode for editing documents using the Super Meta embedded
  DSL (as Embedded in Common Lisp)."
  :syntax-table yard-super-meta-mode-syntax-table
  (define-key paredit-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map
    (kbd "}") 'paredit-close-curly))

(defvar yard-terminal-counter 1)

(defadvice term (after yard-rename-term-buffer first () disable)
  "Rename the buffer created by ``term'' in order to support
multiple buffers created this way."
  (rename-buffer (concat "*terminal-"
                         (number-to-string yard-terminal-counter)
                         "*")
                 t)
  (incf yard-terminal-counter))

(defun yard-sort-words (reverse beginning end)
  "Sort words in region alphabetically. Prefixed with negative
\\[universal-argument], sorts in referse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beginning end))

(defun yard-get-environment-path ()
  "Return a list of the paths in the environment variable PATH."
  (split-string (getenv "PATH") path-separator))
