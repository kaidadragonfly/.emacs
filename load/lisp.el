(require 'flycheck)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.  
   (auto-fill-mode t)
   ;; Enable Flyspell.  
   (flyspell-prog-mode)
   ;; Make newline magically indent.  :)
   (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
   ;; Make smart-tab always indent.
   (defvar smart-tab-always-indent)
   (setq-local smart-tab-always-indent t)
   ;; Use the proper load path.
   (setq flycheck-emacs-lisp-load-path load-path)))
