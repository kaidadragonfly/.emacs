(defun test ()
  "Open the test for the current file."
  (interactive)
  (let ((base (replace-regexp-in-string
               "/src/main/" "/src/test/" buffer-file-name)))
    (find-file (replace-regexp-in-string "[.]scala" "Test.scala" base))))
   
(defun main ()
  "Open the main source for the current file."
  (interactive)
  (let ((base (replace-regexp-in-string
               "/src/test/" "/src/main/" buffer-file-name)))
    (find-file (replace-regexp-in-string "Test[.]scala" ".scala" base))))

;; The following requires emacs 24.
;; Using ignore-errors so that the rest of the config loads on earlier
;; versions.
(ignore-errors
  (add-hook 'scala-mode-hook
            (lambda ()
              (run-hooks 'c-mode-common-hook)
              (flyspell-prog-mode)
              (auto-fill-mode 0)
              (set-fill-column 80)
              (auto-revert-mode t)
              (local-set-key (kbd "RET") 'newline-and-indent)
              ;; Setup scalastyle.
              (defvar flycheck-scalastyle-jar)
              (setq-local flycheck-scalastyle-jar
                          (expand-file-name
                           "~/.emacs.d/lib/scalastyle_2.10-0.6.0-batch.jar"))
              (defvar flycheck-scalastylerc)
              (setq-local flycheck-scalastylerc
                          (concat (proj-root) "/scalastyle-config.xml"))
              ;; Use tags.
              (require 'etags)
              (declare-function proj-root "ide.el" nil)
              ;; Rebuild tags.
              (add-hook 'after-save-hook 'rebuild-tags nil t)
              ;; Only check syntax on load and save
              (defvar flycheck-check-syntax-automatically)
              (setq-local flycheck-check-syntax-automatically '(save))
              ;; Show margin.
              (require 'fill-column-indicator)
              (declare-function fci-mode
                                "fill-column-indicator.el"
                                nil)
              (defvar fci-rule-character-color)
              (setq-local fci-rule-character-color "color-234")
              (if (> (window-width) (current-fill-column))
                  (progn (fci-mode)
                         (toggle-truncate-lines nil))))))

(declare-function rebuild-tags "ide.el")
(eval-after-load "scala-mode" '(rebuild-tags))
