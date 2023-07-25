;; mouse integration
(require 'mwheel)

(when (equal (getenv "TERM_PROGRAM") "iTerm.app")
  (require 'mouse) ;; needed for iterm2 compatibility
  (xterm-mouse-mode)
  (defvar mouse-wheel-progressive-speed)
  (setq mouse-wheel-progressive-speed nil)
  (defvar mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(1))

  (mouse-wheel-mode 1))
