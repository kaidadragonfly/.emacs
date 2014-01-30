(add-hook
 'ruby-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode
   (auto-fill-mode 1)
   ;; Make enter indent.
   (local-set-key "\C-m" 'newline-and-indent)))
