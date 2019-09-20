(define-derived-mode dotenv-mode shell-script-mode ".env"
  "A mode for editing .env files."
  (setq mode-line-process nil)
  (flycheck-mode 0))

(add-to-list 'auto-mode-alist '("\\.env" . dotenv-mode))
