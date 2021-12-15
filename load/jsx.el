(require 'rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'magic-mode-alist '("import.*react" . rjsx-mode))

(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

(defun js-electric-char (arg)
  (interactive "P")

  (self-insert-command (prefix-numeric-value arg))
  (indent-for-tab-command))

(add-hook
 'rjsx-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode.
   (auto-fill-mode)
   ;; Turn off flycheck
   (flycheck-mode 0)
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)
   ;; Make }, > and ; indent.
   (local-set-key (kbd "}") 'js-electric-char)
   (local-set-key (kbd ">") 'js-electric-char)
   (local-set-key (kbd ";") 'js-electric-char)
   ;; Rebuild tags on save.
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags nil t)
   ;; Use xref for definitions.
   (local-set-key (kbd "M-.") 'xref-find-definitions)
   ;; Indent JSX by 2 spaces.
   (defvar sgml-basic-offset)
   (setq-local sgml-basic-offset 2)
   ;; Turn on subword mode.
   (subword-mode)
   (require 'diminish)
   (diminish 'subword-mode)))
