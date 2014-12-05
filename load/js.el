;; Load js2-mode.
(add-to-list 'load-path "~/.emacs.d/lib/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun js-hook-fun ()
  ;; Activate auto-fill-mode.
  (auto-fill-mode t)
  ;; Enable Flyspell.
  (flyspell-prog-mode)
  ;; Allow movement between subwords.
  (subword-mode 1))

(add-hook 'js-mode-hook 'js-hook-fun)
(add-hook 'js2-mode-hook 'js-hook-fun)

;; Setup js2-mode for json
(setq auto-mode-alist
      (cons '("\\.json$" . (lambda () (js2-mode)))
            auto-mode-alist))

(add-hook
 'js2-mode-hook
 (lambda ()
   (local-set-key (kbd "<f8>") 'js2-next-error)))
