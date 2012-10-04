;; Workarounds
(setq flyspell-issue-welcome-flag nil) ; dictionaries-common
(global-set-key (kbd "<select>") 'move-end-of-line)
;; Load custom elisp files.  
(add-to-list 'load-path "~/.emacs.d/lib")
;; Load modular settings.  
(require 'elisp-load-dir)
(elisp-load-dir "~/.emacs.d/load")
;; Reload changed files automatically.
(global-auto-revert-mode)
;;----------------------------------------------------------------------------
;; Interface related items.  
;;----------------------------------------------------------------------------
;; Make file completion case insensitive.
(setq read-file-name-completion-ignore-case t)
;; Display buffers interactively when switching.  
;; Upgraded to ido, which also does files and who knows what else...
(ido-mode t)
;; ido loads tramp on find file, so we may as well do so now.  
(require 'tramp)
;; (iswitchb-default-keybindings)
;; Rectangles :)
(cua-selection-mode t)
(global-set-key (kbd "<C-j>") 'cua-set-rectangle-mark)
;; Iedit mode.
(global-set-key "\M-'" 'iedit-mode)
;; Make line numbers have a space after them.
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
;; Set up winner mode.  
(winner-mode t)
;; Set up meta-arrows to move around the windows.  
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
;; Save all backup files in one directory.  
(if (file-directory-p "~/.emacs.d/backups")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))))
;; Set compilation window height.  
(setq compilation-window-height 0)      ;Hide the window.  
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
