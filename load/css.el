(add-hook
 'css-mode-hook
 (lambda ()
   ;; Format on save.
   (add-hook 'before-save-hook
             (lambda ()
               (whitespace-cleanup)
               (indent-region (point-min) (point-max) nil))
             nil t)))
