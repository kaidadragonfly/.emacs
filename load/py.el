(add-hook
 'python-mode-hook
 (lambda ()
   (require 'flymake)
   ;; Check spelling.
   (flyspell-prog-mode)
   ;; Turn on auto-fill-mode.
   (auto-fill-mode)
   (setq comment-auto-fill-only-comments t) ; Only fill comments.
   ;; Turn on subword mode.
   (subword-mode)
   ;; Make enter indent.
   (local-set-key (kbd "RET") 'newline-and-indent)
   ;; Four spaces, no tabs, don't guess!
   (defvar python-guess-indent)
   (setq tab-width 4
         indent-tabs-mode nil
         python-guess-indent nil)
   ;;
   ;; Setup flymake.
   ;;
   ;; Use pylint.
   (when (load "flymake" t)
     (defun flymake-pylint-init ()
       (declare-function flymake-init-create-temp-buffer-copy "flymake")
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory
                            buffer-file-name))))
         (list "epylint" (list local-file))))

     (defvar flymake-allowed-file-name-masks)
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init)))
   ;; Start flymake on file load.
   (add-hook 'find-file-hook 'flymake-find-file-hook)
   ;; Rebind indent-region to whitespace cleanup
   (local-set-key (kbd "C-M-\\") 'whitespace-cleanup)
   ;; Clean up whitespace on save.
   (add-hook 'before-save-hook 'whitespace-cleanup)))

