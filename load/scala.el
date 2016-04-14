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
                        '(save))
            ;; Disable "scala" checker.
            (defvar flycheck-disabled-checkers "flycheck.el")
            (setq flycheck-disabled-checkers
                  (cons 'scala flycheck-disabled-checkers))
            ;; Show margin.
            (require 'fill-column-indicator)
            (declare-function fci-mode
                              "fill-column-indicator.el"
                              nil)
            (defvar fci-rule-character-color)
            (setq-local fci-rule-character-color "color-234")
            (if (> (window-width) (current-fill-column))
                (progn (fci-mode)
                       (toggle-truncate-lines nil)))
            (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)))

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
   (warning line-start "[warn] "
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
(define-derived-mode sbt-conf-mode scala-mode ".Sbt"
  "A mode for sbt configuration files.")
;; Remove old scala-mode entries.
(setq auto-mode-alist
      (remove (rassoc 'scala-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-conf-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(declare-function end-of-line-p "ide.el" nil)

(require 'scala-mode2)

(defun scala-start-of-parameter-list ()
    (search-forward "(" nil nil))

(defun scala-end-of-parameter-p ()
  (let ((p (char-after (point))))
    (or (= p ?\,)
        (= p ?\)))))

(defun scala-end-of-parameter ()
  "Move point to the end of parameter (i.e., to the comma or
close-parenthesis after it).  Assumes that it is currently on the
type or name."
  (interactive)
  (scala-syntax:skip-forward-ignorable)
  (while (not (scala-end-of-parameter-p))
    (scala-syntax:forward-sexp)
    (scala-syntax:skip-forward-ignorable)))

(defun scala-end-of-parameter-list ()
  (interactive)
  (scala-end-of-parameter)
  (while (= (char-after (point)) ?\,)
    (forward-char)
        (scala-end-of-parameter)))

(defun scala-wrap-parameter ()
  (scala-end-of-parameter)
  (forward-char)
  (unless (end-of-line-p)
    (newline-and-indent)))

(defvar wrap-parameter)
(setq wrap-parameter 'scala-wrap-parameter)
(make-local-variable wrap-parameter)

(defvar start-of-parameter-list)
(setq start-of-parameter-list 'scala-start-of-parameter-list)
(make-local-variable start-of-parameter-list)

(defvar end-of-parameter-list)
(setq end-of-parameter-list 'scala-end-of-parameter-list)
(make-local-variable end-of-parameter-list)

(add-hook
 'sbt-mode-hook
 (lambda ()
   (local-set-key (kbd "<f7>") 'compilation-previous-error)
   (local-set-key (kbd "<f9>") 'compilation-next-error)))
