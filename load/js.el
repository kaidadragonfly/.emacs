;; Load js2-mode.
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun js-hook-fun ()
  ;; Activate auto-fill-mode.
  (set-fill-column 100)
  (auto-fill-mode t)
  ;; Enable Flyspell.
  (flyspell-prog-mode)
  ;; Clean whitespace on save.
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; Rebuild tags on save.
  (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)
  ;; Allow movement between subwords.
  (subword-mode 1)
  (require 'diminish)
  (diminish 'subword-mode))

(add-hook
 'js2-mode-hook
 (lambda ()
   (local-set-key (kbd "<f8>") 'js2-next-error)))
