;; Load custom elisp files.
(add-to-list 'load-path "~/.emacs.d/lib")

;;----------------------------------------------------------------------------
;; Interface related items.
;;----------------------------------------------------------------------------
;; Make file completion case insensitive.
(setq read-file-name-completion-ignore-case t)
;; Display buffers interactively when switching.
(iswitchb-mode t)
(when (fboundp 'iswitchb-default-keybindings)
  (iswitchb-default-keybindings))
;; Make line numbers have a space after them.
(defvar linum-format)
(setq linum-format "%3d ")
;; Highlight parenthesis.
(show-paren-mode 1)
;; Get rid of the initially useful, but ultimately annoying splash screen.
(setq inhibit-startup-message t)
;; Give buffers unique names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; Remove menu bar in text-mode.
(when (and (fboundp 'window-system) (not (window-system))) (menu-bar-mode 0))
;; Reload changed files automatically.
(global-auto-revert-mode)
;; Save all backup files in one directory.
(if (file-directory-p "~/.emacs.d/backups")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))))
;; Set compilation window height.
(setq compilation-window-height 0)      ;Hide the window.
;; Silence flyspell welcome message.
(defvar flyspell-issue-welcome-flag)
(setq flyspell-issue-welcome-flag nil)
;; Follow version control links.
(setq vc-follow-symlinks t)

;;-----------------------------------------------------------------------------
;; Workarounds.
;;-----------------------------------------------------------------------------
;; Sometimes needed for usage inside of tmux.
(global-set-key (kbd "<select>") 'move-end-of-line)

;;-----------------------------------------------------------------------------
;; Load modular settings.
;;-----------------------------------------------------------------------------
(require 'elisp-load-dir)
(elisp-load-dir "~/.emacs.d/load")

;;-----------------------------------------------------------------------------
;; Functions.
;;-----------------------------------------------------------------------------
;; Create a new *scratch* buffer!
(defun scratch-create ()
  "Creates and switches to a new scratch buffer.

Kills the old scratch buffer.  "
  (interactive)
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

;;-----------------------------------------------------------------------------
;; Content related items.
;;-----------------------------------------------------------------------------
;; Require a newline at the end of the file.
(setq require-final-newline t)
;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;;-----------------------------------------------------------------------------
;; Misc.
;;-----------------------------------------------------------------------------
;; Add support for compressed files.
(auto-compression-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(initial-buffer-choice nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit t :height 96 :width normal :family "DejaVu Sans Mono"))))
 '(comint-highlight-prompt ((t (:foreground "white"))))
 '(flymake-errline ((((class color) (background light)) (:background "color-52" :weight bold))))
 '(flymake-warnline ((((class color) (background light)) (:background "color-23" :weight bold))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "magenta"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "green")))))

;; Byte compile elisp files.
(with-temp-message ""
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

