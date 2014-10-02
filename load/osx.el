(if (eq system-type 'darwin)
    (progn
      ;; mouse integration
      (require 'mouse) ;; needed for iterm2 compatibility
      (xterm-mouse-mode)

      (global-set-key (kbd "<mouse-4>") '(lambda ()
                                           (interactive)
                                           (scroll-down
                                            1)))
      (global-set-key (kbd "<mouse-5>") '(lambda ()
                                           (interactive)
                                           (scroll-up
                                            1)))))
