;; Load iedit.
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

(defun keyboard-quit-iedit ()
  "Make C-g disable iedit-mode."
  (iedit-done))
(advice-add 'keyboard-quit :before #'keyboard-quit-iedit)
