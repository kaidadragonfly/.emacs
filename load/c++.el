(add-hook
 'c++-mode-hook
 (lambda ()
   (font-lock-add-keywords 'c++-mode
                           '(("nullptr" . font-lock-constant-face)))))
