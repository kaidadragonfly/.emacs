(defun latex-init ()
   ;; Enable autofill.  
   (turn-on-auto-fill)
   ;; Enable flyspell.  
   (flyspell-mode 1)
   ;; Allow rebinding of flymake functions.  
   (require 'flymake)
   ;; Fix Flymake for LaTeX.  
   (defun flymake-get-tex-args (file-name)
     "Interfaces between chktex and flymake.

Calls chktex with appropriate flags and the correct file-name (passed
in by flymake).  "
     (list "chktex" (list "-q" "-v0" "-n1" "-n6" "-n11" "-n12" "-n13"
                          "-n15" "-n17" "-n26" "-n36""-n37" "-n38"
                          file-name)))
   ;; Enable flymake
   (flymake-mode-on)
   ;; Bind compile to F5.  
   (local-set-key [f5] 'compile)
   ;; And to C-cC-c
   (local-set-key "\C-c\C-c" (lambda () (interactive) (compile compile-command))))


(add-hook 'latex-mode-hook 'latex-init)
(add-hook 'LaTeX-mode-hook 'latex-init)
;; AUCTeX specific settings.  
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Use PDFTeX
            (TeX-PDF-mode 1)
            ;; Enable parsing, because the docs say to.
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            ;; Make math bits green, instead of red.
            (set-face-foreground 'font-latex-math-face "green")
            ;; Make C-cC-s not ask about the label.  
            (setq LaTeX-section-hook '(LaTeX-section-heading
                                       LaTeX-section-title
                                       LaTeX-section-section))))

