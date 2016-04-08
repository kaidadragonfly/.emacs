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
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defun require-package (pkg)
  "Guarantee that `pkg` is installed."
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

(require-package 'js2-mode)
(require-package 'scala-mode2)
(require-package 'sbt-mode)
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
(require-package 'elixir-mode)
(require-package 'haskell-mode)
