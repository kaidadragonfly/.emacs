;; The following requires emacs 24.
;; Using ignore-errors so that the rest of the config loads on earlier
;; versions.
(ignore-errors
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'scala-mode2)
    (package-refresh-contents)
    (package-install 'scala-mode2))

  (unless (package-installed-p 'fill-column-indicator)
    (package-refresh-contents)
    (package-install 'fill-column-indicator))

  (add-hook 'scala-mode-hook
            (lambda ()
              (run-hooks 'c-mode-common-hook)
              (require 'flymake)
              (flyspell-prog-mode)
              (auto-fill-mode 0)
              (set-fill-column 80)
              (subword-mode)
              (auto-revert-mode t)
              ;; Make enter indent.
              (local-set-key (kbd "RET") 'newline-and-indent)
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
