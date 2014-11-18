(require 'java-mode-indent-annotations)

(add-hook
 'java-mode-hook
 (lambda ()
   ;; Show line numbers.
   ;; (linum-mode t)
   ;; Support annotations.
   (java-mode-indent-annotations-setup)
   (setq auto-save-timeout 1)))

;; When setting up flymake use: ecj -Xemacs -d none SrcFile.java
