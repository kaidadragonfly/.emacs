;; Load custom elisp files.
(add-to-list 'load-path "~/.emacs.d/lib")

;;----------------------------------------------------------------------------
;; Interface related items.
;;----------------------------------------------------------------------------
;; Make file completion case insensitive.
(setq read-file-name-completion-ignore-case t)
;; Display buffers interactively when switching.
(iswitchb-mode t)
(iswitchb-default-keybindings)
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
;; Reload changed files automatically.
(global-auto-revert-mode)
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
;; Silence flyspell welcome message.
(setq flyspell-issue-welcome-flag nil)

;; Workarounds.
;; Sometimes needed for usage inside of tmux.
(global-set-key (kbd "<select>") 'move-end-of-line)

;; Load modular settings.
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
