;; Install various packages.
(if (>= emacs-major-version 24)
    (progn
      (require 'package)
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize)

      (unless (package-installed-p 'groovy-mode)
        (package-refresh-contents)
        (package-install 'groovy-mode))

      (unless (package-installed-p 'js2-mode)
        (package-refresh-contents)
        (package-install 'js2-mode))
      
      (unless (package-installed-p 'scala-mode2)
	(package-refresh-contents)
	(package-install 'scala-mode2))

      (unless (package-installed-p 'markdown-mode)
	(package-refresh-contents)
	(package-install 'markdown-mode))

      (unless (package-installed-p 'dockerfile-mode)
	(package-refresh-contents)
	(package-install 'dockerfile-mode))

      (unless (package-installed-p 'iedit)
	(package-refresh-contents)
	(package-install 'iedit))

      (unless (package-installed-p 'fill-column-indicator)
	(package-refresh-contents)
	(package-install 'fill-column-indicator))

      (unless (package-installed-p 'flycheck)
	(package-refresh-contents)
	(package-install 'flycheck))))
