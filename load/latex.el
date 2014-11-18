(defun latex-init ()
  ;; Enable autofill.
  (turn-on-auto-fill)
  ;; Enable flyspell.
  (flyspell-mode 1)
  ;; Bind compile to F5.
  (local-set-key (kbd "<f5>") 'compile)
  ;; And to C-cC-c
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (compile compile-command))))

(add-hook 'latex-mode-hook 'latex-init)
(add-hook 'LaTeX-mode-hook 'latex-init)
;; AUCTeX specific settings.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Use PDFTeX
            (defvar TeX-PDF-mode)
            (setq TeX-PDF-mode t)
            ;; Enable parsing, because the docs say to.
            (defvar TeX-auto-save)
            (defvar TeX-parse-self)
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            ;; Make math bits green, instead of red.
            (set-face-foreground 'font-latex-math-face "green")
            ;; Make C-cC-s not ask about the label.
            (defvar LaTeX-section-hook)
            (setq LaTeX-section-hook '(LaTeX-section-heading
                                       LaTeX-section-title
                                       LaTeX-section-section))))
