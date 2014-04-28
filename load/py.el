(add-hook
 'python-mode-hook
 (lambda ()
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode.
   (auto-fill-mode)
   (setq comment-auto-fill-only-comments t) ; Only fill comments.
   ;; Turn on subword mode.
   (subword-mode)
   ;; Make enter indent.
   (local-set-key "\C-m" 'newline-and-indent)
   (local-set-key (kbd "<backtab>") (lambda ()
                                      "Un-indent the current line."
                                      (interactive)
                                      (back-to-indentation)
                                      (python-backspace 1)))
   ;; Four spaces, no tabs, don't guess!
   (setq tab-width 4
         indent-tabs-mode nil
         python-guess-indent nil)
   ;;
   ;; Setup flymake.
   ;;
   ;; Use pylint.
   (when (load "flymake" t)
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory
                            buffer-file-name))))
         (list "epylint" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init)))
   ;; Start flymake on file load.
   (add-hook 'find-file-hook 'flymake-find-file-hook)))
