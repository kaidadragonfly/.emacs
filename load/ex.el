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
   ;; Bind M-. and M-/
   (defvar alchemist-mode-map)
   (define-key alchemist-mode-map (kbd "M-.") 'xref-find-definitions)
   ;; Bind C-m (newline) to `electric-indent-just-newline`
   (defvar elixir-mode-map)
   (define-key elixir-mode-map (kbd "C-m") 'newline-and-indent)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   ;; Rebuild tags after save.
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)
   ;; Allow movement between subwords.
   (subword-mode 1)
   (require 'diminish)
   (diminish 'subword-mode)
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp
               (rx symbol-start (or "do" "end") symbol-end))))


