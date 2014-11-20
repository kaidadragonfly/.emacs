;; The following requires emacs 24.
;; Using ignore-errors so that the rest of the config loads on earlier
;; versions.
(ignore-errors
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
                (toggle-truncate-lines nil))))))
