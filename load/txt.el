(add-to-list 'auto-mode-alist '("\\.txt$" . text-mode))
(add-hook
 'text-mode-hook
 (lambda ()
   ;; Setup spell checking.
   (flyspell-mode 1)
   ;; Setup hard line wrapping.
   (auto-fill-mode 1)
   ;; Disable flycheck.
   (flycheck-mode 0)
   ;; Setup tabs/indentation.
   (set (make-local-variable 'tab-width) 4)
   (make-local-variable 'tab-stop-list)
   (setq tab-stop-list (let ((stops '(4)))
                         (while (< (car stops)
                                   120)
                           (setq stops (cons (+ 4 (car stops)) stops)))
                         (nreverse stops)))
   (setq-local comment-start "#")
   (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face)))))
