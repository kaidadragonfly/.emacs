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
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)
   ;; Make }, > and ; indent.
   (local-set-key (kbd "}") 'js-electric-char)
   (local-set-key (kbd ">") 'js-electric-char)
   (local-set-key (kbd ";") 'js-electric-char)
   ;; Turn on subword mode.
   (subword-mode)
   (require 'diminish)
   (diminish 'subword-mode)
   ;; Disable Flycheck (rjsx-mode provides js2 style checking)
   (flycheck-mode 0)))
