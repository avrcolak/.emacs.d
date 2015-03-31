;; Simple rc to configure Visual Basic mode.

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                 visual-basic-mode)) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
