(unless (fboundp 'markdown-mode)
    (ignore-errors
      (require 'package)
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize)

      (unless (package-installed-p 'markdown-mode)
        (package-refresh-contents)
        (package-install 'markdown-mode))))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
