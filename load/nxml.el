(autoload 'sgml-indent-line "sgml-mode")

(defun nxml-indent-with-sgml ()
  "Indent using sgml mode for nxml mode."
  (sgml-mode)
  (sgml-indent-line)
  (nxml-mode))

(add-hook
 'nxml-mode-hook
 (lambda ()
   ;; Activate auto-fill-mode.
   (auto-fill-mode t)
   ;; Indent 4 spaces.
   (defvar sgml-basic-offset)
   (setq sgml-basic-offset 4)

   (advice-add 'nxml-indent-line :after #'nxml-indent-with-sgml)

   ;; Enable Flyspell.
   (flyspell-prog-mode)
   (defvar flyspell-prog-text-faces)
   (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
   ;; Add "automatic" indentation.
   (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))
