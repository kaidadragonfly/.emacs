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
   (defvar flycheck-disabled-checkers "flycheck.el")
   (setq flycheck-disabled-checkers
         (cons 'chef-foodcritic flycheck-disabled-checkers))))
