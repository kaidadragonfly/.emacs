(add-hook
 'c-mode-hook
 (lambda ()
   (require 'flymake)
   ;; Custom C flymake rule.  
   ;; Does not require a makefile, only a compiler.  
   ;; (defun flymake-c-init ()
   ;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
   ;;                        'flymake-create-temp-inplace))
   ;;          (local-file  (file-relative-name
   ;;                        temp-file
   ;;                        (file-name-directory buffer-file-name))))
   ;;     (list "clang" (list "-Wall" "-Wextra" "-fsyntax-only" 
   ;;                         "-fno-caret-diagnostics" local-file))))
   ;; (push '(".+\\.c$" flymake-c-init) flymake-allowed-file-name-masks)
   ;; Activate flymake-mode.  
   (when (file-exists-p "Makefile")
     (flymake-mode-on))))
