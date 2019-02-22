(add-hook
 'ruby-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Don't autofill
   (auto-fill-mode 0)
   ;; Make enter indent.
   (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup nil t)
   ;; Rebuild tags on save.
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags nil t)
   ;; Disable rubocop
   (defvar flycheck-disabled-checkers "flycheck.el")
   (setq flycheck-disabled-checkers
         (cons 'ruby-rubocop flycheck-disabled-checkers))
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp "\\(^\\|[[:space:]]\\)\\(do\\|end\\)\\b")))
