(eval-when-compile (require 'cl))

(add-hook
 'elixir-mode-hook
 (lambda ()
   ;; Compile alchemist in a different directory.
   (make-local-variable 'process-environment)
   (pushnew "ALCHEMIST_MODE=1" process-environment)
   (alchemist-mode t)
   (require 'flycheck-mix)
   (flycheck-mix-setup)
   ;; Bind alchemist-help-search-at-point to F1
   (local-set-key (kbd "<f1>") 'alchemist-help-search-at-point)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp
               (rx symbol-start (or "do" "end") symbol-end))))
