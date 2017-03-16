;; Don't perform syntax checks for .html.erb files.
(define-derived-mode html-erb-mode web-mode "html-erb-mode"
  "A mode for .html.erb files.")

(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . html-erb-mode))

(add-hook
 'html-erb-mode-hook
 (lambda ()
   (auto-fill-mode 0)))
