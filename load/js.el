;; Load js2-mode.
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook
 'js2-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.
   (set-fill-column 100)
   (auto-fill-mode t)
   ;; Enable Flyspell.
   (flyspell-prog-mode)
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)
   ;; Clean whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup nil t)
   ;; Rebuild tags on save.
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags nil t)
   ;; Make f8 move to the next error.
   (local-set-key (kbd "<f8>") 'js2-next-error)
   ;; Allow movement between subwords.
   (subword-mode 1)
   (require 'diminish)
   (diminish 'subword-mode)))
