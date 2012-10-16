(require 'java-mode-indent-annotations)

(add-hook
 'java-mode-hook
 (lambda ()
   ;; Show line numbers.
   ;; (linum-mode t)
   ;; Support annotations.
   (java-mode-indent-annotations-setup)
   ;; Flymake mode :)
   (flymake-mode t)
   ;; Auto save the file, when saving a backup.
   ;; (add-hook 'auto-save-hook 'save-buffer-if-visiting-file)
   ;; Auto save when idle for one second.
   (setq auto-save-timeout 1)))

;; When setting up flymake use: ecj -Xemacs -d none SrcFile.java

;; Flymake stub.  Beginnings of making flymake not use a makefile for java
;; (defun my-java-flymake-init ()
;;   (list "javac" (list (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-with-folder-structure))))
