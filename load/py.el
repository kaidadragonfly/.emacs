(add-hook
 'python-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode.
   (auto-fill-mode)
   (setq comment-auto-fill-only-comments t) ; Only fill comments.
   ;; Turn on subword mode.
   (subword-mode)
   (let ((entry (assq 'subword-mode minor-mode-alist)))
     (when entry (setcdr entry '(nil))))
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)
   ;; Four spaces, no tabs, don't guess!
   (defvar python-guess-indent)
   (setq tab-width 4
         indent-tabs-mode nil
         python-guess-indent nil)
   ;; Rebind indent-region to whitespace cleanup
   (local-set-key (kbd "C-M-\\") 'whitespace-cleanup)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)
   (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)
   (setq-local comment-inline-offset 2)))

(require 'flycheck)
(flycheck-add-next-checker 'python-flake8 'python-pylint)
