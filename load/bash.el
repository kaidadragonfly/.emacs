;; Setup shell-script-mode for .bash_*
(setq auto-mode-alist
      (cons '("\\.bash_\\w*$" . (lambda () (shell-script-mode)))
            auto-mode-alist))
