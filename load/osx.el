(defun osx-smart-copy-region ()
  (interactive)
  (cua-copy-region nil)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "pbcopy")
  (message ""))

;; mouse integration
(require 'mwheel)

(when (equal (getenv "TERM_PROGRAM") "iTerm.app")
  (require 'mouse) ;; needed for iterm2 compatibility
  (xterm-mouse-mode)
  (defvar mouse-wheel-progressive-speed)
  (setq mouse-wheel-progressive-speed nil)
  (defvar mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(1))

  (mouse-wheel-mode 1)

  (declare-function cua-copy-region (arg) "cua-base.el")
  (global-set-key (kbd "M-w") 'osx-smart-copy-region))
