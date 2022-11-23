(add-hook
 'elixir-mode-hook
 (lambda ()
   (defvar elixir-mode-map)
   (define-key elixir-mode-map (kbd "C-m") 'newline-and-indent)
   (defun elixir-format-quietly ()
     (interactive)
     (let ((inhibit-message t))
       (elixir-format)))
   ;; Format on save.
   (add-hook 'before-save-hook 'elixir-format-quietly nil t)
   ;; Rebuild tags after save.
   ;; (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags nil t)
   ;; Instead of indenting region format file.
   (define-key elixir-mode-map (kbd "C-M-\\") 'elixir-format)
   ;; Allow movement between subwords.
   (subword-mode 1)
   (require 'diminish)
   (diminish 'subword-mode)
   ;; Enable LSP
   ;; TODO: Configure LSP the way I want it first.
   ;; (lsp)

   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp
               (rx symbol-start (or "do" "end") symbol-end))))
