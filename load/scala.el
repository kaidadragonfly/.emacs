;; The following requires emacs 24.
;; Using ignore-errors so that the rest of the config loads on earlier
;; versions.
(ignore-errors
  ;; Split out a sbt-config-mode from scala-mode.
  ;; (this is mostly to disable flycheck for sbt files.)
  (define-derived-mode scala-config-mode scala-mode "Sbt-Config"
    "A mode for editing .sbt files."
    nil)

  (add-hook
   'scala-mode-hook
   (lambda ()
     (run-hooks 'c-mode-common-hook)
     (flyspell-prog-mode)
     (auto-fill-mode 0)
     (set-fill-column 80)
     (subword-mode)
     (auto-revert-mode t)
     (local-set-key (kbd "RET") 'newline-and-indent)
     ;; Use tags.
     (declare-function proj-root "ide.el" nil)
     (setq-local tags-file-name (concat (proj-root) "/.tags"))
     ;; Rebuild tags.
     (add-hook 'find-file-hook 'rebuild-tags nil t)
     (add-hook 'after-save-hook 'rebuild-tags nil t)
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
     ;; Switch to scala-config-mode if .sbt file.
     (require 'rx)
     (declare-function string/ends-with ".ide.el" str suffix)
     (if (string/ends-with buffer-file-name ".sbt")
         (progn
           (declare-function scala-config-mode "scala.el")
           (scala-config-mode)) nil))))
