(add-hook
 'elixir-mode-hook
 (lambda ()
   (alchemist-mode t)
   (require 'flycheck-mix)
   (flycheck-mix-setup)
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp "\\(^\\|[[:space:]]\\)\\(do\\|end\\)\\b")))
