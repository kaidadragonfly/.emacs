(require 'flycheck)

(defun touch-and-check-buffer ()
  "Touch and save the buffer before checking syntax."
  (interactive)
  (save-excursion
    (end-of-line nil)
    (insert " ")
    (save-buffer)
    (flycheck-buffer)))

(global-set-key (kbd "<f7>") 'flycheck-previous-error)
(global-set-key (kbd "<f8>") 'touch-and-check-buffer)
(global-set-key (kbd "<f9>") 'flycheck-next-error)
