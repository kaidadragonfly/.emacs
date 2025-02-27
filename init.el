;;; -*- lexical-binding: t -*-
(defvar default-gc-threshold)
(setq default-gc-threshold gc-cons-threshold)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq-default gc-cons-threshold (* 64 1024 1024))

;; Initialize packages.
(load "~/.emacs.d/ide")
(load "~/.emacs.d/load")

;; Load secrets.
(if (file-exists-p "~/.emacs.d/secrets.el")
    (load "~/.emacs.d/secrets"))

;;----------------------------------------------------------------------------
;; Interface related items.
;;----------------------------------------------------------------------------
;; Make file completion case insensitive.
(setq read-file-name-completion-ignore-case t)
;; Make line numbers have a space after them.
(setq-default linum-format "%3d ")
;; Disable `electric-indent-mode`
(electric-indent-mode 0)
;; Highlight parenthesis.
(show-paren-mode 1)
;; Get rid of the initially useful, but ultimately annoying splash screen.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; Give buffers unique names.
(eval-when-compile (require 'uniquify))
(setq uniquify-buffer-name-style 'forward)
;; Remove menu bar in text-mode.
(when (and (fboundp 'window-system) (not (window-system))) (menu-bar-mode 0))
;; Reload changed files automatically.
(global-auto-revert-mode)
;; Don't show messages when using minibuffer:
;; https://emacs.stackexchange.com/questions/46690/prevent-showing-buffer-reversion-message-while-working-in-the-minibuffer
(advice-add
 'auto-revert-handler
 :around (lambda (orig-fun &rest args)
           (setq-default auto-revert-verbose (not (minibufferp (window-buffer))))
           (apply orig-fun args)))
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
(setq compilation-window-height 0)      ; Hide the window.
;; Silence flyspell welcome message.
(setq-default flyspell-issue-welcome-flag nil)
;; Follow version control links.
(setq vc-follow-symlinks t)
;; Save file location between runs.
(eval-when-compile (require 'saveplace))
(if (fboundp 'save-place-mode)
    (save-place-mode +1)
  (setq-default save-place t))

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
 '(c-default-style '((java-mode . "bsd") (awk-mode . "awk") (other . "gnu")))
 '(case-fold-search t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(confirm-kill-emacs nil)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(terminal-colors))
 '(custom-safe-themes
   '("44e4c9c9bccfc6f280471d60b1a38ca3f75de94345d10432958d2a15e686f4e8"
     default))
 '(flycheck-checkers
   '(ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine
              chef-foodcritic coffee coffee-coffeelint coq css-csslint
              d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang
              eruby-erubis fortran-gfortran go-gofmt go-golint go-vet
              go-build go-test go-errcheck haml handlebars haskell-ghc
              haskell-hlint html-tidy javascript-jshint
              javascript-eslint javascript-gjslint json-jsonlint less
              lua make perl perl-perlcritic php php-phpmd php-phpcs
              puppet-parser puppet-lint python-flake8 python-pylint
              racket rpm-rpmlint rst rst-sphinx ruby-rubocop
              ruby-rubylint ruby ruby-jruby rust-cargo sass scala
              scala-scalastyle scss sh-bash sh-posix-dash
              sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex
              tex-lacheck texinfo typescript-tslint verilog-verilator
              xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-idle-change-delay 1.0)
 '(flycheck-python-pylint-executable nil)
 '(flycheck-shellcheck-excluded-warnings '("1090" "1091" "2001" "2012" "2129"))
 '(git-commit-summary-max-length 50)
 '(global-flycheck-mode t nil (flycheck))
 '(initial-buffer-choice nil)
 '(ispell-highlight-face 'flyspell-incorrect)
 '(js-indent-level 2)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(js2-include-node-externs t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-skip-preprocessor-directives t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-auto-execute-action nil)
 '(lsp-auto-guess-root t)
 '(lsp-elixir-dialyzer-enabled nil)
 '(lsp-enable-folding nil)
 '(lsp-file-watch-threshold 5000)
 '(lsp-lens-enable nil)
 '(lsp-modeline-code-actions-segments '(count))
 '(lsp-semantic-tokens-enable t)
 '(lsp-ui-doc-alignment 'window)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client
     mode-line-modified mode-line-remote
     mode-line-frame-identification mode-line-buffer-identification
     "   " mode-line-position "  " mode-line-modes mode-line-misc-info
     mode-line-end-spaces))
 '(package-selected-packages
   '(toml-mode sql-indent scss-mode sbt-mode json-mode iedit haskell-mode
               flycheck-rust fill-column-indicator feature-mode
               etags-select dockerfile-mode apples-mode))
 '(paren-face-modes
   '(lisp-mode emacs-lisp-mode lisp-interaction-mode ielm-mode
               scheme-mode inferior-scheme-mode clojure-mode
               cider-repl-mode nrepl-mode arc-mode inferior-arc-mode
               elixir-mode ruby-mode))
 '(ruby-deep-indent-paren nil)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 1)
 '(scroll-bar-mode 'right)
 '(sql-indent-offset 2)
 '(tls-checktrust t)
 '(treemacs-no-delete-other-windows nil)
 '(treemacs-no-png-images t)
 '(vc-follow-symlinks t)
 '(warning-suppress-types '((comp) (comp) (comp) (comp)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-control-block-indentation t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-tests-directory "/Users/kaida/css-presentation/tests/")
 '(xref-prompt-for-identifier t)
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "white"))))
 '(company-tooltip-scrollbar-track ((t (:background "wheat" :foreground "black"))))
 '(cua-rectangle ((t (:inherit region :background "magenta" :foreground "white"))))
 '(custom-button-pressed-unraised ((t (:inherit custom-button-unraised :foreground "magenta"))))
 '(custom-changed ((t (:background "blue" :foreground "white"))))
 '(custom-comment-tag ((t (:foreground "blue"))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "blue" :weight bold :height 1.2))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold))))
 '(elixir-atom-face ((t (:foreground "blue"))))
 '(elixir-attribute-face ((t (:foreground "magenta"))))
 '(flyspell-duplicate ((t nil)))
 '(flyspell-incorrect ((t (:foreground "brightred" :underline t))))
 '(font-lock-builtin-face ((t (:foreground "brightmagenta"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "brightblue"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "magenta" :weight bold))))
 '(font-lock-string-face ((t (:foreground "blue"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(font-lock-warning-face ((t (:foreground "brightyellow" :underline t :weight bold))))
 '(ido-subdir ((t (:foreground "brightblue"))))
 '(iedit-read-only-occurrence ((t (:inherit region :foreground "brightwhite"))))
 '(isearch ((t (:background "magenta" :foreground "white"))))
 '(isearch-fail ((t (:background "red" :foreground "white"))))
 '(lazy-highlight ((t (:background "brightcyan" :foreground "black"))))
 '(lsp-ui-doc-background ((t (:background "color-237"))))
 '(match ((t (:background "brightyellow" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(parenthesis ((t (:foreground "brightblack"))))
 '(region ((t (:background "yellow" :foreground "brightwhite"))))
 '(secondary-selection ((t (:background "yellow"))))
 '(show-paren-match ((t (:background "cyan" :foreground "brightwhite"))))
 '(smerge-markers ((t (:background "white" :foreground "black"))))
 '(smerge-mine ((t (:background "#660000"))))
 '(smerge-other ((t (:background "#336633"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-22"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "color-88"))))
 '(vc-annotate-face-CCCCFF ((t (:background "#CCCCFF" :foreground "black"))) t)
 '(vc-annotate-face-CCE4FF ((t (:background "#CCE4FF" :foreground "color-16"))) t)
 '(vc-annotate-face-FFCCCC ((t (:background "#FFCCCC" :foreground "black"))) t)
 '(warning ((t (:foreground "color-214" :weight bold))))
 '(web-mode-block-delimiter-face ((t (:foreground "magenta"))))
 '(web-mode-block-face ((t (:background "LightYellow1" :foreground "black"))))
 '(web-mode-doctype-face ((t (:foreground "magenta" :weight bold))))
 '(web-mode-html-attr-name-face ((t (:foreground "brightcyan"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "white"))))
 '(web-mode-html-tag-face ((t (:foreground "cyan"))))
 '(web-mode-inlay-face ((t (:background "LightYellow1" :foreground "black"))))
 '(web-mode-symbol-face ((t (:foreground "brightblue")))))

;; Only use one window when opening multiple files.
(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

;; Make spelling handle camel-case
(setq-default ispell-program-name "aspell")
;; -C makes aspell accept run-together words
;; --run-together-limit is maximum number of words that can be
;; strung together.
(setq-default ispell-extra-args '("-C" "--sug-mode=ultra" "--run-together-limit=5"))
;; Disable eldoc (it interferes with flycheck).
(global-eldoc-mode 0)
;; Load paren-face-mode.
(global-paren-face-mode 1)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (run-with-timer
    0.1
    nil
    (lambda ()
      (message "Emacs ready in %s with %d garbage collections."
               (format "%.2f seconds"
                       (float-time
                        (time-subtract after-init-time before-init-time)))
               gcs-done)))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold default-gc-threshold)
