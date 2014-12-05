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
   (subword-mode 1)
   (let ((entry (assq 'subword-mode minor-mode-alist)))
     (when entry (setcdr entry '(nil))))
   ;; Bind compile to F5.
   (local-set-key (kbd "<f5>") 'compile)
   ;; And to C-cC-c
   (local-set-key (kbd "C-c C-c") 'compile)
   ;; Setup indentation.
   (defvar c-basic-offset)
   (setq c-basic-offset 4)
   ;; Have tabs (in C mode) mirror c-indentation.
   (set (make-local-variable 'tab-width) c-basic-offset)
   ;; Electric paired braces, parens, etc.
   ;; (require 'autopair)
   ;; (setq autopair-blink nil)
   ;; (autopair-mode t)
   ;; Make newline automatically indent the next line.
   (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
   ;; Clean whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Make smart-tab always indent.
   (defvar smart-tab-always-indent)
   (set (make-local-variable 'smart-tab-always-indent) t)))

