;; Install various packages.
(defun require-package (pkg)
  "Guarantee that `pkg` is installed."
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))


(require 'package)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(require-package 'apples-mode)
(require-package 'auto-package-update)
(require-package 'company)
(require-package 'diminish)
(require-package 'eglot)
(require-package 'elixir-mode)
(require-package 'feature-mode)
(require-package 'fill-column-indicator)
(require-package 'flycheck)
(require-package 'flycheck-rust)
(require-package 'git-commit)
(require-package 'iedit)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'markdown-mode)
(require-package 'nginx-mode)
(require-package 'paren-face)
(require-package 'projectile)
(require-package 'racket-mode)
(require-package 'rjsx-mode)
(require-package 'rust-mode)
(require-package 's)
(require-package 'sql-indent)
(require-package 'swift-mode)
(require-package 'typescript-mode)
(require-package 'web-mode)
(require-package 'yaml-mode)
;; (require-package 'alchemist)
