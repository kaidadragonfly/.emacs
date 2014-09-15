(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.  
   (auto-fill-mode t)
   ;; Enable Flyspell.  
   (flyspell-prog-mode)
   ;; Make newline magically indent.  :)
   (local-set-key "\C-m" 'reindent-then-newline-and-indent)
   ;; Make smart-tab always indent.
   (defvar smart-tab-always-indent)
   (setq-local smart-tab-always-indent t)))
