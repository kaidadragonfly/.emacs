;; Settings to be loaded for C mode, and descendants.  
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; Activate auto-fill-mode.  
   (auto-fill-mode 1)
   ;; Activate flyspell-prog-mode.  
   (flyspell-prog-mode)
   ;; Deactivate Abbrev mode.  
   (abbrev-mode 0)
   ;; Allow movement between subwords.  
   (when (fboundp 'c-subword-mode)
     (c-subword-mode 1))
   (when (fboundp 'subword-mode)
     (subword-mode 1))   
   ;; Bind compile to F5.  
   (local-set-key [f5] 'compile)
   ;; And to C-cC-c
   (local-set-key "\C-c\C-c" 'compile)
   ;; Setup indentation.  
   (setq c-basic-offset 4)
   ;; Have tabs (in C mode) mirror c-indentation.  
   (set (make-local-variable 'tab-width) c-basic-offset)
   ;; Make enter indent.  
   (local-set-key "\C-m" 'reindent-then-newline-and-indent)))
