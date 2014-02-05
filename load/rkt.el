;; Setup scheme-mode for .rkt
(setq auto-mode-alist
      (cons '("\\.rkt$" . (lambda () (scheme-mode)))
                        auto-mode-alist))
