;;; Simple rc to configure Org-mode.

(when (require 'org-mode nil t)
  ;; Goodbye latex preview droppings!
  (setq org-latex-preview-ltxpng-directory "/tmp/org1000/")

  ;; Execute these languages from an Org-mode buffer.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     ;; "lisp" in this case strictly means Common Lisp.
     (lisp . t)
     ;; C in this case means both C++ and C. Confusingly, the only
     ;; language specifiers recoginized by babble are C, C++ and
     ;; cpp. The first of which doesn't do anything on my system, and
     ;; the second of which does not have syntax highlighting. Only
     ;; source blocks with the language specified as cpp behave as
     ;; expected.
     (C . t)
     (perl . t)
     (haskell . t)
     (dot . t)
     (sparql . t)))

  ;; Enable syntax highlighting in Org-mode code blocks.
  (setq org-src-fontify-natively t)

  (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("smalltalk" . smalltalk))
  (add-to-list 'org-src-lang-modes '("mathematica" . math))

  ;; Convenience over safety.
  (setq org-confirm-babel-evaluate nil)

  ;; Make windmove work in Org-mode, at uncertain cost.
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))
