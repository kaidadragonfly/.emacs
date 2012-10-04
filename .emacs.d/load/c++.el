(add-hook
 'c++-mode-hook
 (lambda ()
   (font-lock-add-keywords 'c++-mode
                           '(("nullptr" . font-lock-constant-face)))
   ;; Activate flymake-mode.  
   (require 'flymake)
   (when (file-exists-p "Makefile")
     (flymake-mode-on))))
