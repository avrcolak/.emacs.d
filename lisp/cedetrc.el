;;; A simple rc to configure CEDET.
(require 'cedet)
(require 'semantic)
(require 'srecode)

(setq ede-locate-setup-options '(ede-locate-logbal
                                 ede-locate-locate
                                 ede-locate-base))
(global-ede-mode 1)

(global-srecode-minor-mode 1)

(setq semantic-default-submodes
      (append semantic-default-submodes
              '(global-semantic-decoration-mode
                global-semantic-highlight-func-mode
                global-semantic-idle-completions-mode
                global-semantic-idle-local-symbol-highlight-mode
                global-semantic-idle-summary-mode
                global-semantic-show-unmatched-syntax-mode
                global-semantic-mru-bookmark-mode)))

(semantic-mode 1)

(setq speedbar-use-images nil)
