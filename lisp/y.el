;;; Y. Useful Emacs Lisp.

(defmacro y-swap-vars (var1 var2)
  "Swaps VAR1 and VAR2."
  `(setq ,var1 (prog1 ,var2 (setq ,var2 ,var1))))

(defun y-union (&rest sets)
  "Returns the union of SETS."
  (deletedups (apply #'nconc (mapcar (copy-sequence sets)))))

(defun y-set-difference (minuend subtrahend)
  (let ((minuend (copy-sequence minuend)))
    (dolist (element subtrahend minuend) (delq element minuend))))

(defun y-keys (alist)
  (mapcar #'car alist))

(defun y-assq-all (keys alist)
  (delq nil (mapcar (lambda (key) (assq key alist)) keys)))

(defmacro y-flet (bindings &rest body)
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

(defmacro y-with-point-at-click (function)
  "Expands to a command which first moves the point to the
position clicked on with the mouse, and then calls FUNCTION
interactively."
  `(lambda (event)
     (interactive "@e")
     (mouse-set-point event)
     (call-interactively ,function)))

(defun y-add-hooks (hooks function)
  "Add to the value of each hook in HOOKS the function FUNCTION
as if by `add-hook'."
  (dolist (hook hooks) (add-hook hook function)))

(defun y-get-environment-path ()
  "Return a list of the paths in the environment variable PATH."
  (split-string (getenv "PATH") path-separator))

(defun y-find-subdirectories (directories)
  "Returns a list of the subdirectories for each directory in DIRECTORIES."
  (delq nil
        (mapcar (lambda (name) (and (file-directory-p name) name))
                (let (subdirectories '())
                  (dolist (directory directories subdirectories)
                    (setq subdirectories
                          (nconc (directory-files directory t "[[:word:]]+")
                                 subdirectories)))))))

(defun y-do-nothing ()
  "Does nothing and returns non-nil."
  (interactive) t)

(defun y-display-prefix (arg)
  "Display the value of the raw prefix ARG."
  (interactive "P")
  (message "%s" arg))

(defun y-quit-other-window (&optional kill)
  "Quits the other window as if by `quit-window'"
  (interactive)
  (quit-window kill (next-window)))

(defun y-sort-words (reverse beginning end)
  "Sort words in region alphabetically. Prefixed with negative
\\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beginning end))

(defun y-toggle-window-dedication ()
  "Toggles whether the window is dedicated. See Info node
`Dedicated Windows'."
  (interactive)
  (let ((window (selected-window)))
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (message (if (window-dedicated-p window)
                 "Window %s dedicated"
               "Window %s not dedicated")
             window)))

(defun y-browse-url-no-switch (url &rest args)
  "Like `browse-url', but ensure that input focus doesn't leave
the current frame."
  (interactive "i")
  (let ((frame (selected-frame)))
    (if (called-interactively-p)
        (call-interactively 'browse-url)
      (apply #'browse-url url args))
    ;; HACK: At least one browser (Firefox) does not provide a way to
    ;; remotley open a URL in the foreground, without the browser also
    ;; stealing focus after a small delay. So, steal it back.
    (sleep-for 0 500)
    (select-frame-set-input-focus frame)))

(defun y-slovnik-lookup ()
  "Looks up the word at point on URL `http://slovnik.seznam.cz'"
  (interactive)
  (y-browse-url-no-switch (concat "http://slovnik.seznam.cz/cz-en/word/?q="
                                     (url-encode-url (word-at-point)))))

(defun y-enclose-region-in-src-block ()
  "Enclose the lines in the active region with #+BEGIN_SRC and
#+END_SRC."
  (interactive)
  (when (region-active-p)
    (let ((beginning (region-beginning))
          (end (region-end)))
      (when (> beginning end) (y-swap-vars beginning end))
      (goto-char end)
      (end-of-line)
      (insert "\n#+END_SRC")
      (goto-char beginning)
      (beginning-of-line)
      (insert "#+BEGIN_SRC\n")
      (backward-char))))

(defvar y-root-find-file-hook nil
  "List of functions to be called after a buffer is loaded from a
file with `y-root-find-file'.")

(defun y-root-find-file ()
  "Edit file as the root user."
  (interactive)
  (find-file (concat "/sudo:root@localhost:"
                     (read-file-name "Find file (as root): ")))
  (run-hooks y-root-find-file-hook))

(defvar y-meta-mode-syntax-table
  (let ((table (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\@ "'" table)
    (modify-syntax-entry ?\$ "'" table)
    (modify-syntax-entry ?\! "'" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table used in `y-meta-mode'.")

(define-derived-mode y-meta-mode lisp-mode
  "Meta"
  "Major mode for editing documents using the Meta embedded
DSL (as Embedded in Common Lisp)."
  :syntax-table y-meta-mode-syntax-table
  (define-key paredit-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key paredit-mode-map
    (kbd "}") 'paredit-close-curly))

(defun y-package-install (package)
  "Makes best effort to install PACKAGE as if with
`package-install'. Returns PACKAGE if successful, otherwise nil."
  (if (package-installed-p package)
      package
    (unless package-archive-contents
      (package-refresh-contents))
    (and (ignore-errors (package-install package)) package)))

(defun y-pseudo-require (package &optional noerror)
  "Install PACKAGE as if with `y-package-install'. If unsuccessful,
attempt to `load' the printname of PACKAGE concatenated with
\"-autoloads\".

Normally return PACKAGE."
  (or (y-package-install package)
      (and (load (concat (symbol-name package) "-autoloads") noerror)
           package)))

(defvar y-buffer-windows '())

(defun y-display-buffer-previous-window (buffer alist)
  "Displays BUFFER in a window previously showing a buffer with
the same name, and displayed with this command.

If no such window exists, displays BUFFER as if with
`display-buffer-popup-window'."
  (let* ((buffer-name (buffer-name buffer))
         (assoc (assoc buffer-name y-buffer-windows))
         (previous-window (cdr assoc))
         (window
          (if (and (window-live-p previous-window) previous-window)
              (display-buffer-in-previous-window
               buffer
               (append alist `((previous-window . ,previous-window))))
            (display-buffer-pop-up-window buffer alist))))
    (setq y-buffer-windows (delete assoc y-buffer-windows))
    (push (cons buffer-name window) y-buffer-windows)
    window))

(defvar y-auto-minor-mode-alist ()
  "Alist of file name patterns vs corresponding minor mode
functions. Closely mimics `auto-mode-alist'.")

(defun y-set-auto-minor-mode ()
  "Select minor modes appropriate for curent buffer.

To find the right minor modes, this function compares the
filename against all entries in `y-auto-minor-mode-alist' and
enables the specified minor modes."
  (when buffer-file-name
    (let ((remote-id (file-remote-p buffer-file-name))
          (name buffer-file-name))
      ;; Clean up the file name for this buffer.
      (setq name (file-name-sans-versions name))
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (dolist (entry y-auto-minor-mode-alist)
        (when (and (car entry) (cdr entry))
          (if (string-match (car entry) name)
              (funcall (cdr entry))))))))
