(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(add-hook
 'web-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.
   (auto-fill-mode t)
   ;; Enable Flyspell.
   (flyspell-prog-mode)
   ;; Use visual-line-mode instead of auto-fill-mode
   (auto-fill-mode 0)
   (visual-line-mode 1)
   ;; Indent on save.
   (add-hook 'before-save-hook
             (lambda ()
               (whitespace-cleanup)
               (indent-region (point-min) (point-max) nil))
             nil t)
   ;; Add "automatic" indentation.
   (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(require 'flycheck)
(flycheck-add-mode 'html-tidy 'web-mode)
(flycheck-add-mode 'css-stylelint 'web-mode)
