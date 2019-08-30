(add-hook
 'swift-mode-hook
 (lambda ()
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)))
