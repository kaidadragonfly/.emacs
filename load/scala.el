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
              ;; Use tags.
              (require 'etags)
              ;; Only check syntax on load and save
              (defvar flycheck-check-syntax-automatically)
              (setq-local flycheck-check-syntax-automatically
                          '(mode-enabled save))
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

;; Define a sbt checker!
(require 'flycheck)
(flycheck-define-checker sbt
  "Checker for compilation with SBT"
  :command ("esbt")
  :error-patterns
  ((error line-start "[error] " (file-name) ":" line ": "
          (message (zero-or-more not-newline)
                   (one-or-more "\n" blank
                                (zero-or-more not-newline)))
          line-end)
   (warn line-start "[warn] " (file-name) ":" line ": "
         (message (zero-or-more not-newline)
                  (one-or-more "\n" blank
                               (zero-or-more not-newline)))
         line-end))
  :modes scala-mode)

(flycheck-add-next-checker 'sbt 'scala)

;; Don't perform syntax checks for sbt configuration files.
(define-derived-mode sbt-mode scala-mode "Sbt"
  "A mode for sbt configuration files.")
;; Remove old scala-mode entries.
(setq auto-mode-alist
      (remove (rassoc 'scala-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
