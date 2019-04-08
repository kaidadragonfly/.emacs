(add-hook
 'mhtml-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.
   (auto-fill-mode t)
   ;; Enable Flyspell.
   (flyspell-prog-mode)
   ;; Add "automatic" indentation.
   (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
