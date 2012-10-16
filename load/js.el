(add-hook
 'js-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.  
   (auto-fill-mode t)
   ;; Enable Flyspell.  
   (flyspell-prog-mode)
   ;; Make newline magically indent.  :)
   (local-set-key "\C-m" 'reindent-then-newline-and-indent)
   ;; Allow movement between subwords.  
   (when (fboundp 'c-subword-mode)
     (c-subword-mode 1))
   (when (fboundp 'subword-mode)
     (subword-mode 1))))
