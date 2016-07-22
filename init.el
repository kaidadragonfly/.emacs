;; Load custom elisp files.
(add-to-list 'load-path "~/.emacs.d/lib")
;; Load legacy compatability.
(load "~/.emacs.d/lib/legacy-compat.el")
;; Initialize packages.
(load "~/.emacs.d/install-packages")
(load "~/.emacs.d/ide")

;;----------------------------------------------------------------------------
;; Interface related items.
;;----------------------------------------------------------------------------
;; Make file completion case insensitive.
(setq read-file-name-completion-ignore-case t)
;; Make line numbers have a space after them.
(defvar linum-format)
(setq linum-format "%3d ")
;; Highlight parenthesis.
(show-paren-mode 1)
;; Get rid of the initially useful, but ultimately annoying splash screen.
(setq inhibit-startup-message t)
;; Give buffers unique names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; Remove menu bar in text-mode.
(when (and (fboundp 'window-system) (not (window-system))) (menu-bar-mode 0))
;; Reload changed files automatically.
(global-auto-revert-mode)
;; Save all backup & autosave files in one directory.
(if (file-directory-p "~/.emacs.d/backups")
    (progn
      (setq backup-directory-alist
            '((".*" . "~/.emacs.d/backups/")))
      (setq auto-save-list-file-prefix "~/.emacs.d/backups/")
      (setq auto-save-file-name-transforms
            '((".*" "~/.emacs.d/backups/" t)))))
;; Disable interlocking, prevent dropping of hidden symlinks.
(setq create-lockfiles nil)

;; Set compilation window height.
(setq compilation-window-height 0)      ;Hide the window.
;; Silence flyspell welcome message.
(defvar flyspell-issue-welcome-flag)
(setq flyspell-issue-welcome-flag nil)
;; Follow version control links.
(setq vc-follow-symlinks t)
;; Save file location between runs.
(require 'saveplace)
(setq-default save-place t)

;;-----------------------------------------------------------------------------
;; Workarounds.
;;-----------------------------------------------------------------------------
;; Sometimes needed for usage inside of tmux.
(global-set-key (kbd "<select>") 'move-end-of-line)

;;-----------------------------------------------------------------------------
;; Load modular settings.
;;-----------------------------------------------------------------------------
(require 'elisp-load-dir)
(elisp-load-dir "~/.emacs.d/load")

;;-----------------------------------------------------------------------------
;; Functions.
;;-----------------------------------------------------------------------------
;; Create a new *scratch* buffer!
(defun scratch-create ()
  "Creates and switches to a new scratch buffer.

Kills the old scratch buffer.  "
  (interactive)
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

;;-----------------------------------------------------------------------------
;; Content related items.
;;-----------------------------------------------------------------------------
;; Require a newline at the end of the file.
(setq require-final-newline t)
;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)

;;-----------------------------------------------------------------------------
;; Misc.
;;-----------------------------------------------------------------------------
;; Add support for compressed files.
(auto-compression-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((java-mode . "bsd")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(case-fold-search t)
 '(confirm-kill-emacs nil)
 '(flycheck-checkers
   (quote
    (scala-syncheck ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-idle-change-delay 1.0)
 '(flycheck-python-pylint-executable "epylint")
 '(flycheck-shellcheck-excluded-warnings (quote ("1090" "1091" "2001" "2012" "2129")))
 '(global-flycheck-mode t nil (flycheck))
 '(initial-buffer-choice nil)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(js-indent-level 2)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("describe" "it")))
 '(js2-include-node-externs t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-skip-preprocessor-directives t)
 '(js2-strict-missing-semi-warning nil)
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(ruby-deep-indent-paren nil)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 1)
 '(scroll-bar-mode (quote right))
 '(sql-indent-offset 2)
 '(tls-checktrust t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-control-block-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 0)
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "white"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "magenta"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "green"))))
 '(ido-subdir ((t (:foreground "brightblue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-240")))))

;; Only use one window when opening multiple files.
(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

;; Byte compile elisp files.
(defvar-local init-bc "")

(declare-function string/starts-with "ide.el" str, prefix)
(if (string/starts-with
     (byte-recompile-directory (expand-file-name "~/.emacs.d") 0 nil)
     "Done (Total of 0 files compiled")
    nil
  (load-file "~/.emacs.d/init.el"))

;; Make spelling handle camel-case
(defvar ispell-program-name)
(defvar ispell-extra-args)
(setq ispell-program-name "aspell")
;; -C makes aspell accept run-together words
;; --run-together-limit is maximum number of words that can be
;; strung together.
(setq ispell-extra-args '("-C" "--sug-mode=ultra" "--run-together-limit=5"))
