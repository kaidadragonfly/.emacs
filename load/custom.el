;;-----------------------------------------------------------------------------
;; Emacs's "custom" system.
;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(initial-buffer-choice nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right)))
;; Fix default text size (it is ginormous!).
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit t :height 96 :width normal :family "DejaVu Sans Mono"))))
 '(flymake-errline ((((class color) (background light)) (:background "color-52" :weight bold))))
 '(flymake-warnline ((((class color) (background light)) (:background "color-23" :weight bold))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "magenta"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "green")))))
(put 'set-goal-column 'disabled nil)
