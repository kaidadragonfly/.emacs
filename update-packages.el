(package-initialize)

(load "~/.emacs.d/install-packages")

(require 'auto-package-update)
(setq auto-package-update-interval 3)
(auto-package-update-maybe)
