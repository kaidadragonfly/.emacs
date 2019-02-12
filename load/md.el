(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-unset-key (kbd "<backtab>"))
   (defvar markdown-indent-on-enter "markdown-mode.el")
   (setq markdown-indent-on-enter nil)
   ;; Clean whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Use visual-line-mode instead of auto-fill-mode
   (auto-fill-mode 0)
   (visual-line-mode 1)
   ;; Disable flycheck
   (flycheck-mode 0)
   ;; Disable company-mode
   (company-mode 0)))
