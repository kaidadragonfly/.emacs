(add-hook
 'elixir-mode-hook
 (lambda ()
   (alchemist-mode t)
   (require 'flycheck-mix)
   (flycheck-mix-setup)
   ;; Bind alchemist-help-search-at-point to F1
   (local-set-key (kbd "<f1>") 'alchemist-help-search-at-point)
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp "\\(^\\|[[:space:]]\\)\\(do\\|end\\)\\b")))
