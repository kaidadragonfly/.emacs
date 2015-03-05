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
            ;; Disable "scala" checker.
            (defvar flycheck-disabled-checkers "flycheck.el")
            (setq flycheck-disabled-checkers (cons 'scala flycheck-disabled-checkers))
            ;; Show margin.
            (require 'fill-column-indicator)
            (declare-function fci-mode
                              "fill-column-indicator.el"
                              nil)
            (defvar fci-rule-character-color)
            (setq-local fci-rule-character-color "color-234")
            (if (> (window-width) (current-fill-column))
                (progn (fci-mode)
                       (toggle-truncate-lines nil)))))

;; Define a sbt checker!
(require 'flycheck)
(flycheck-define-checker sbt
  "Checker for compilation with SBT"
  :command ("esbt")
  :error-patterns
  ((error line-start "[error] "
          ;; Ignore column info for now, since scalastyle uses 0
          ;; indexed columns, instead of one indexed, and it's
          ;; non-trivial to work around.
          (file-name) ":" line ":" (optional (one-or-more digit) ":") " "
          (message (zero-or-more not-newline)
                   (zero-or-more "\n" blank (zero-or-more not-newline)))
          line-end)
   (warn line-start "[warn] "
         (file-name) ":" line ":" (optional (one-or-more digit) ":") " "
         (message (zero-or-more not-newline)
                  (zero-or-more "\n" blank (zero-or-more not-newline)))
         line-end))
  :modes scala-mode)

(flycheck-add-next-checker 'sbt 'scala)


;; Rebuild tags on save.
(declare-function rebuild-tags "ide.el")
(eval-after-load "scala-mode" '(rebuild-tags))

;; Don't perform syntax checks for sbt configuration files.
(define-derived-mode sbt-mode scala-mode "Sbt"
  "A mode for sbt configuration files.")
;; Remove old scala-mode entries.
(setq auto-mode-alist
      (remove (rassoc 'scala-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
