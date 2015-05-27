(if (eq system-type 'darwin)
    (progn
      ;; mouse integration
      (require 'mouse) ;; needed for iterm2 compatibility
      (xterm-mouse-mode)

      (global-set-key (kbd "<mouse-4>") '(lambda ()
                                           (interactive)
                                           (scroll-down 1)))
      (global-set-key (kbd "<mouse-5>") '(lambda ()
                                           (interactive)
                                           (scroll-up 1)))
      (global-set-key (kbd "<prior>") '(lambda ()
                                         (interactive)
                                         (insert " ")))
      (declare-function cua-copy-region (arg) "cua-base.el")
      (defun smart-copy-region ()
        (interactive)
        (cua-copy-region nil)
        (shell-command-on-region
         (region-beginning)
         (region-end)
         "pbcopy")
        (message ""))
      (global-set-key (kbd "M-w") 'smart-copy-region)))
