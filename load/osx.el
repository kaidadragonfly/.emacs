;; mouse integration
(require 'mouse) ;; needed for iterm2 compatibility
(require 'mwheel)

(if (equal (getenv "TERM_PROGRAM") "iTerm.app")
    (progn
      (xterm-mouse-mode)
      (defvar mouse-wheel-progressive-speed)
      (setq mouse-wheel-progressive-speed nil)
      (defvar mouse-wheel-scroll-amount)
      (setq mouse-wheel-scroll-amount '(1))

      (mouse-wheel-mode 1)
      
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
