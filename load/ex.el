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
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags nil t)
   ;; Instead of indenting region format file.
   (define-key elixir-mode-map (kbd "C-M-\\") 'elixir-format)
   ;; Allow movement between subwords.
   (subword-mode 1)
   (require 'diminish)
   (diminish 'subword-mode)
   ;; Disable flycheck, enable eglot
   (flycheck-mode 0)
   (eglot-ensure)
   (require 'seq)
   (setq
    mode-line-misc-info
    (seq-filter
     (lambda (pair) (not (eql (car pair) 'eglot--managed-mode)))
     mode-line-misc-info))
   ;; Make do/end less prominent.
   (defvar paren-face-regexp)
   (setq-local paren-face-regexp
               (rx symbol-start (or "do" "end") symbol-end))))

(require 'eglot)
(add-to-list
 'eglot-server-programs
 '(elixir-mode . ("sh" "/opt/homebrew/bin/elixir-ls")))
