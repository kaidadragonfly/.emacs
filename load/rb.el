(add-hook
 'ruby-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Don't autofill
   (auto-fill-mode 0)
   ;; Make enter indent.
   (local-set-key (kbd "C-m") 'newline-and-indent)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Rebuild tags on save.
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)
   ;; Disable foodcritic, because our chef files are sad.
   (defvar flycheck-disabled-checkers "flycheck.el")
   (setq flycheck-disabled-checkers
         (cons 'chef-foodcritic flycheck-disabled-checkers))
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp "do\\|end")))
