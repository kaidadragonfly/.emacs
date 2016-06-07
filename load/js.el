;; Load js2-mode.
(add-to-list 'load-path "~/.emacs.d/lib/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(defun js-hook-fun ()
  ;; Activate auto-fill-mode.
  (set-fill-column 80)
  (auto-fill-mode t)
  ;; Enable Flyspell.
  (flyspell-prog-mode)
  ;; Clean whitespace on save.
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; Allow movement between subwords.
  (subword-mode 1)
  (let ((entry (assq 'subword-mode minor-mode-alist)))
    (when entry (setcdr entry '(nil)))))

(add-hook 'js-mode-hook 'js-hook-fun)
(add-hook 'js2-mode-hook 'js-hook-fun)
(add-hook 'web-mode-hook 'js-hook-fun)

(add-hook
 'js2-mode-hook
 (lambda ()
   (local-set-key (kbd "<f8>") 'js2-next-error)))

(require 'flycheck)
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
