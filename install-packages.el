;; Install various packages.
(defun require-package (pkg)
  "Guarantee that `pkg` is installed."
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))


(lambda ()
  (require 'package)

  (setq package-archives
        '(("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "http://elpa.gnu.org/packages/")))

  ;; (require-package 'alchemist)
  (require-package 'apples-mode)
  (require-package 'company)
  (require-package 'diminish)
  (require-package 'elixir-mode)
  (require-package 'feature-mode)
  (require-package 'fill-column-indicator)
  (require-package 'flycheck)
  (require-package 'flycheck-mix)
  (require-package 'flycheck-rust)
  (require-package 'git-commit)
  (require-package 'iedit)
  (require-package 'js2-mode)
  (require-package 'json-mode)
  (require-package 'markdown-mode)
  (require-package 'nanowrimo)
  (require-package 'paren-face)
  (require-package 'projectile)
  (require-package 'rjsx-mode)
  (require-package 'rust-mode)
  (require-package 's)
  (require-package 'sql-indent)
  (require-package 'typescript-mode)
  (require-package 'use-package)
  (require-package 'web-mode))
