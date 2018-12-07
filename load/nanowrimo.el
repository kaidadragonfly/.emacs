(require 'nanowrimo)

(add-hook
 'nanowrimo-mode-hook
 (lambda ()
   ;; Make F5 submit count.
   (local-set-key (kbd "<f5>") 'nanowrimo-update-nanowrimo-org)))

(defun nanowrimo-today-goal-from-date ()
  "Update today's goal from the current date."
  (setq nanowrimo-today-goal
        (ceiling (* (/ (float nanowrimo-total-goal) nanowrimo-num-days)
                    (nanowrimo-days-into-nanowrimo)))))
