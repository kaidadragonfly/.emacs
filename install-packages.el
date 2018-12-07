;; Install various packages.
(require 'package)

(defvar tls-program)
(defvar gnutls-verify-error)
(defvar gnutls-trustfiles)
(let ((trustfile
       (replace-regexp-in-string
	"\\\\" "/"
	(replace-regexp-in-string
	 "\n" ""
	 (shell-command-to-string "python -m certifi")))))
  (setq tls-program
	(list
	 (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
		 (if (eq window-system 'w32) ".exe" "") trustfile))))

(let ((trustfile
       (replace-regexp-in-string
	"\\\\" "/"
	(replace-regexp-in-string
	 "\n" ""
	 (shell-command-to-string "python -m certifi")))))
  (setq tls-program
	(list
	 (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
		 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; (eval-when-compile
;;   (require 'cl)
;;   (let ((bad-hosts
;; 	 (loop for bad
;; 	       in `("https://wrong.host.badssl.com/"
;; 		    "https://self-signed.badssl.com/")
;; 	       if (condition-case e
;; 		      (url-retrieve
;; 		       bad (lambda (retrieved) t))
;; 		    (error nil))
;; 	       collect bad)))
;;     (if bad-hosts
;; 	(error (format "tls misconfigured; retrieved %s ok"
;; 		       bad-hosts))
;;       (url-retrieve "https://badssl.com"
;; 		    (lambda (retrieved) t)))))

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(defun require-package (pkg)
  "Guarantee that `pkg` is installed."
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(require-package 'alchemist)
(require-package 'apples-mode)
(require-package 'diminish)
(require-package 'elixir-mode)
(require-package 'feature-mode)
(require-package 'fill-column-indicator)
(require-package 'flycheck)
(require-package 'flycheck-mix)
(require-package 'flycheck-rust)
(require-package 'git-commit)
(require-package 'haskell-mode)
(require-package 'iedit)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'markdown-mode)
(require-package 'nanowrimo)
(require-package 'paren-face)
(require-package 'projectile)
(require-package 'rust-mode)
(require-package 's)
(require-package 'sql-indent)
