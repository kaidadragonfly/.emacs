(autoload 'diminish "diminish")

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-unset-key (kbd "<backtab>"))
   (defvar markdown-indent-on-enter)
   (setq markdown-indent-on-enter nil)
   ;; Clean whitespace on save (except in /Developer/).
   (add-hook 'before-save-hook
             (lambda ()
               (unless
                   (string-match "/Developer/" default-directory)
                 (whitespace-cleanup))) nil t)
   ;; Use visual-line-mode instead of auto-fill-mode
   (auto-fill-mode 0)
   (visual-line-mode 1)
   ;; Disable flycheck
   (flycheck-mode 0)
   ;; Disable company-mode
   (company-mode 0)
   ;; Turn on subword mode.
   (subword-mode)
   (diminish 'subword-mode)
   ;; Highlight code blocks
   (defvar markdown-fontify-code-blocks-natively)
   (setq markdown-fontify-code-blocks-natively t)
   ;; Make <f5> compile.
   (local-set-key (kbd "<f5>") 'compile)
   (defvar compilation-read-command)
   (setq-local compilation-read-command nil)
   (setq-local compile-command
               (concat "gfm "
                       (if buffer-file-name
                           (shell-quote-argument
                            (file-name-nondirectory
                             buffer-file-name)))))))
