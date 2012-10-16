(add-hook
 'nxml-mode-hook
 (lambda ()
   ;; Enable autofill.  
   (turn-on-auto-fill)
   ;; Activate auto-fill-mode.  
   (auto-fill-mode t)
   ;; Enable Flyspell.  
   (flyspell-prog-mode)
   (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
   ;; Add "automatic" indentation.   
   (local-set-key "\C-m" 'reindent-then-newline-and-indent)))
;; Setup xml-mode for xul
(setq auto-mode-alist
      (cons '("\\.xul$" . (lambda () (xml-mode)))
            auto-mode-alist))
(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)
