(add-hook
 'typescript-mode-hook
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
  ;; Allow movement between subwords.
  (subword-mode 1)
  (require 'diminish)
  (diminish 'subword-mode)))
