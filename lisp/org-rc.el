;;; Simple rc to configure org.

(y-pseudo-require 'org-plus-contrib)

(eval-after-load "org"
  '(progn 
     ;; Goodbye latex preview droppings!
     (setq org-latex-preview-ltxpng-directory "/tmp/org1000/")

     ;; Execute these languages from an org-mode buffer.
     (org-babel-do-load-languages
      'org-babel-load-languages
      `((scheme . t)
        ;; "lisp" in this case strictly means Common Lisp.
        (lisp . t)
        ;; C in this case means both C++ and C. Confusingly, the only
        ;; language specifiers recoginized by babble are C, C++ and
        ;; cpp. The first of which doesn't do anything on my system(s),
        ;; and the second of which does not have syntax
        ;; highlighting. Only source blocks with the language specified
        ;; as cpp behave as expected.
        (C . t)
        (perl . t)
        (haskell . t)
        (dot . t)
        ;; ob-sparql does not (yet) come with org.
        ,(when (locate-library "ob-sparql") '(sparql . t))))

     (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
     (add-to-list 'org-src-lang-modes '("mathematica" . wolfram))

     ;; Convenience over safety.
     (setq org-confirm-babel-evaluate nil)

     ;; Make windmove work in org-mode, at uncertain cost.
     (add-hook 'org-shiftup-final-hook 'windmove-up)
     (add-hook 'org-shiftleft-final-hook 'windmove-left)
     (add-hook 'org-shiftdown-final-hook 'windmove-down)
     (add-hook 'org-shiftright-final-hook 'windmove-right)

     (setq org-completion-use-ido t)

     ;; org-plus-contrib also provides (only) org, but with contribs.
     (when (package-installed-p 'org-plus-contrib)
       ;; HACK: Empirically, requiring org-drill doesn't load cl prior to
       ;; attempting to compile `copy-list'. So, require cl manually.
       (when (require 'cl nil t)
         (require 'org-drill nil t)))))
