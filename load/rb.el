(add-hook
 'ruby-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode
   (auto-fill-mode 1)
   ;; Make enter indent.
   (local-set-key (kbd "C-m") 'newline-and-indent)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)))
