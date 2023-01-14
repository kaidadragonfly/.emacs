(define-derived-mode heex-mode web-mode
  "HEEX"
  "A variant of web-mode.")

(add-hook
 'heex-mode-hook
 (lambda ()
   ;; Format on save.
   (add-hook 'before-save-hook 'elixir-format-quietly nil t)
   ;; Instead of indenting region format file.
   (define-key heex-mode-map (kbd "C-M-\\") 'elixir-format)
   ;; Enable LSP.
   (defvar lsp-language-id-configuration)
   (with-eval-after-load 'lsp-mode
     (add-to-list 'lsp-language-id-configuration
               '(heex-mode . "elixir")))

   (lsp)))

(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-mode))
