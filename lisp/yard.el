;;; The Emacs Lisp YARD: Yet Another Run command Directory. Some
;;; useful Emacs Lisp functions for my Emacs rc files.

(defun yard-find-subdirectories (directories) 
  "Returns a list of the subdirectories for each directory in DIRECTORIES."
  (delq nil
        (mapcar (lambda (x) (and (file-directory-p x) x))
                (apply 'append
                       (let (subdirectories '())
                         (dolist (directory directories subdirectories)
                           (setq subdirectories
                                 (cons (directory-files directory
                                                        t
                                                        "[[:word:]]+")
                                       subdirectories))))))))

;;; Helpers for setting up buffer local Lisp indentation.
(defun yard-set-lisp-indent (indent-function)
  (set (make-local-variable 'lisp-indent-function) indent-function))

(defun yard-set-elisp-indent ()
  (yard-set-lisp-indent 'lisp-indent-function))

(defun yard-set-common-lisp-indent ()
  (yard-set-lisp-indent 'common-lisp-indent-function))

;;; System for opening a file as root with tramp.
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
  (eval-when-compile (require 'tramp))
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
  "Send the code form you want to SLIME (Do What I Mean).
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

(defvar yard-meta-mode-syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\@ "'" table)
    (modify-syntax-entry ?\$ "'" table)
    (modify-syntax-entry ?\! "'" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table used in `yard-meta-mode'.")

(define-derived-mode yard-meta-mode lisp-mode
  "Meta"
  "Major mode for editing documents using the Meta embedded
DSL (as Embedded in Common Lisp)."
  :syntax-table yard-meta-mode-syntax-table
  (define-key paredit-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map
    (kbd "}") 'paredit-close-curly))

(defun yard-sort-words (reverse beginning end)
  "Sort words in region alphabetically. Prefixed with negative
\\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beginning end))

(defun yard-get-environment-path ()
  "Return a list of the paths in the environment variable PATH."
  (split-string (getenv "PATH") path-separator))

(defun yard-package-install (feature)
  (when (require 'package nil t)
    (unless package--initialized
      (package-initialize)
      (add-to-list 'package-archives
                   '("melpa-stable" . "http://stable.melpa.org/packages/") t))
    (unless package-archive-contents
      (package-refresh-contents))
    (unless (package-installed-p feature)
      (ignore-errors (package-install feature)))))

(defun yard-require (feature &optional filename)
  "If FEATURE is not loaded, load it as if with `require'. If
unsuccessful, install the package FEATURE as with
`package-install' and try again.

When FEATURE is successfuly loaded the return value is FEATURE,
otherwise nil."
  (or (require feature filename t)
      (and (yard-package-install feature)
           (require feature filename t))))

(defun yard-locate-feature (feature)
  "Show the precise filename which provides FEATURE. If
unsuccessful, require FEATURE as with `yard-require' and try
again.

If FEATURE cannot be located, the return value is nil."
  ;; Emacs Lisp doesn't have flet.
  (let ((locate-feature
         (lambda (feature)
           (find-lisp-object-file-name feature
                                       (symbol-function feature)))))
    (or (funcall locate-feature feature)
        (and (yard-require feature)
             (funcall locate-feature feature)))))

(defun yard-optimistically-locate-feature (feature &optional filename)
  "If FILENAME is nil (which is the default) Locate FEATURE as
with `locate-library' using the FEATURE's symbol name, otherwise,
use FILENAME. If unsuccessful, install the package FEATURE as
with `yard-package-install' and try again.

When successful show the precise filename where FEATURE is
assumed to be provided, otherwise nil."
  (let ((feature-name (or filename (symbol-name feature))))
    (or (locate-library feature-name)
        (and (yard-package-install feature)
             (locate-library feature-name)))))

(defmacro yard-flet (bindings &rest body)
  "Make temporary dynamic function bindings as if by `fset'.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (let ((symbol-functions-symbol (make-symbol "symbol-functions")))
    `(let ((,symbol-functions-symbol
            (list ,@(mapcar (lambda (binding)
                              (let ((func (car binding)))
                                `(cons ',func (symbol-function ',func))))
                            bindings))))
       ,@(mapcar (lambda (binding)
                   (let ((func (car binding))
                         (arglist (cadr binding))
                         (body (cddr binding)))
                     `(fset ',func (lambda ,arglist ,@body))))
                 bindings)
       (prog1
           (progn ,@body)
         (dolist (binding ,symbol-functions-symbol)
           (fset (car binding) (cdr binding)))))))

(defun yard-do-nothing ()
  "Does nothing and returns t."
  (interactive) t)

(defun yard-quit-other-window (&optional kill)
  "Quits the other window as if by `quit-window'"
  (interactive)
  (quit-window kill (next-window)))

(defun yard-add-hooks (hooks function)
  "Add to the value of each hook in HOOKS the function FUNCTION
as if by `add-hook'."
  (dolist (hook hooks) (add-hook hook function)))
