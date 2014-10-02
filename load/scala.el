;; The following requires emacs 24.
;; Using ignore-errors so that the rest of the config loads on earlier
;; versions.
(ignore-errors
  (require 'package)
  (add-to-list 'package-archives
               '("melpa"
                 . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'scala-mode2)
    (package-refresh-contents) (package-install 'scala-mode2))

  (add-hook
   'scala-mode-hook
   (lambda ()
     (run-hooks 'c-mode-common-hook)))
  (add-hook 'scala-mode-hook
            (lambda ()
              (require 'flymake)
              (flyspell-prog-mode)
              (set-fill-column 80)
              (auto-fill-mode)
              (subword-mode)
              (auto-revert-mode t)
              ;; Make enter indent.
              (local-set-key (kbd "RET") 'newline-and-indent))))
