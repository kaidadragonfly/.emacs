(use-package nanowrimo
  :commands nanowrimo-days-into-nanowrimo
  :config
  (defun nanowrimo-today-goal-from-date ()
    "Update today's goal from the current date."
    (setq nanowrimo-today-goal
          (ceiling (* (/ (float nanowrimo-total-goal) nanowrimo-num-days)
                      (min (nanowrimo-days-into-nanowrimo) 30)))))
  (add-hook
   'nanowrimo-mode-hook
   (lambda ()
     ;; Make F5 submit count.
     (local-set-key (kbd "<f5>") 'nanowrimo-update-nanowrimo-org))))
