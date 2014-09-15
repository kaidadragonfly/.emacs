;; Load iedit.
(add-to-list 'load-path "~/.emacs.d/lib/iedit")
(require 'iedit)

(defun iedit-global (&optional arg)
  "Starts iedit but uses \\[narrow-to-region] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (if iedit-mode
            (iedit-done)
          (iedit-mode))))))

(defun iedit-local (&optional arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; Shrink scope.
        (narrow-to-defun)

        (if iedit-mode
            (iedit-done)
          (iedit-mode))))))

;; Bind to M-'.
(global-set-key (kbd "M-'") 'iedit-local)

;; Bind to M-"
(global-set-key (kbd "M-\"") 'iedit-global)

;; Make C-g disable iedit-mode.
(defadvice keyboard-quit (before keyboard-quit-iedit activate)
  (iedit-done))
