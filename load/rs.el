(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(require 'flycheck)
(flycheck-add-next-checker 'rust-cargo 'rust)

(add-hook
 'rust-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.
   (auto-fill-mode 1)
   ;; Allow movement between subwords.
   (subword-mode 1)
   (require 'diminish)
   (diminish 'subword-mode)
   ;; Activate flyspell-prog-mode.
   (flyspell-prog-mode)
   ;; Make newline automatically indent the next line.
   (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)
   ;; Clean whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Make smart-tab always indent.
   (defvar smart-tab-always-indent)
   (set (make-local-variable 'smart-tab-always-indent) t)))
