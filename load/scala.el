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
                       (toggle-truncate-lines nil)))
            (add-hook (make-local-variable 'after-save-hook) 'rebuild-tags)))

;; Define a custom scala checker!
(require 'flycheck)
(flycheck-define-checker scala-syncheck
  "Checker for compilation with sbt and scalac"
  :command ("scala-syncheck")
  :error-patterns
  ((error line-start
          (file-name) ":" line ": error: "
          (message (zero-or-more not-newline)
                   (zero-or-more "\n" blank (zero-or-more not-newline)))
          line-end)
   (warning line-start
            (file-name) ":" line ": warn: "
            (message (zero-or-more not-newline)
                     (zero-or-more "\n" blank (zero-or-more not-newline)))
            line-end))
  :modes scala-mode)

;; Disabled for now until subprojects work properly.
;; (flycheck-add-next-checker 'scala-syncheck 'scala)
(add-to-list 'flycheck-disabled-checkers 'scala-syncheck)

;; Remove built in scala checkers.
(add-to-list 'flycheck-disabled-checkers 'scala)
(add-to-list 'flycheck-disabled-checkers 'scala-scalastyle)

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
