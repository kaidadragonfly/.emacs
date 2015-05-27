;; Install various packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun require-package (pkg)
  "Guarantee that `pkg` is installed."
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(require-package 'groovy-mode)
(require-package 'js2-mode)
(require-package 'scala-mode2)
(require-package 'markdown-mode)
(require-package 'json-mode)
(require-package 'dockerfile-mode)
(require-package 'iedit)
(require-package 'fill-column-indicator)
(require-package 'flycheck)
(require-package 'sql-indent)
(require-package 'sbt-mode)
(require-package 'yaml-mode)
(require-package 's)
(require-package 'feature-mode)
(require-package 'toml-mode)
