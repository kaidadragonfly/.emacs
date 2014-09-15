(add-hook
 'nxml-mode-hook
 (lambda ()
   ;; Enable autofill.  
   (turn-on-auto-fill)
   ;; Activate auto-fill-mode.  
   (auto-fill-mode t)
   ;; Indent 4 spaces.
   (defvar sgml-basic-offset)
   (setq sgml-basic-offset 4)
   (defadvice nxml-indent-line (after nxml-indent-line activate)
     (sgml-mode)
     (sgml-indent-line)
     (sgml-mode))
   ;; Enable Flyspell.  
   (flyspell-prog-mode)
   (defvar flyspell-prog-text-faces)
   (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
   ;; Add "automatic" indentation.   
   (local-set-key "\C-m" 'reindent-then-newline-and-indent)))

;; Setup xml-mode for xul
(setq auto-mode-alist
      (cons '("\\.xul$" . (lambda () (xml-mode)))
            auto-mode-alist))

;; Setup html-mode for html
(setq auto-mode-alist
      (cons '("\\.html$" . (lambda () (html-mode)))
            auto-mode-alist))

(fset 'html-mode 'nxml-mode)
(fset 'xml-mode 'nxml-mode)
