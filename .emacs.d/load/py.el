(add-hook
 'python-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode
   (auto-fill-mode 1)
   ;; Make enter indent.
   (local-set-key "\C-m" 'newline-and-indent)
   (local-set-key (kbd "<backtab>") (lambda ()
                                      "Un-indent the current line."
                                      (interactive)
                                      (back-to-indentation)
                                      (python-backspace 1)))
   ;; Setup flymake
   (when (load "flymake" t)
     (defun flymake-pyflakes-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory
                            buffer-file-name))))
         (list "pyflakes" (list local-file))))
     
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pyflakes-init)))
   (add-hook 'find-file-hook 'flymake-find-file-hook)))
