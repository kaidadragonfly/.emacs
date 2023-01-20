;; Features to make emacs more competitive with IDEs.
(require 'dabbrev)
(require 'diminish)

;; Sometimes the VC system doesn't fully load and errors without this function.
(defun vc-git-root (arg))
;; Handle git commits nicely.
(autoload 'git-commit-mode "git-commit" nil t)
(add-to-list 'auto-mode-alist
             (cons
              "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
              'git-commit-mode))

;; Handle .gitignore nicely.
(add-to-list 'auto-mode-alist
             (cons "/\\.gitignore\\'" 'conf-unix-mode))

;; Handle .gitconfig and .git/config nicely.
(add-to-list 'auto-mode-alist
             (cons "/\\.git/config\\'" 'conf-unix-mode))
(add-to-list 'auto-mode-alist
             (cons "/\\.gitconfig\\'" 'conf-unix-mode))

;; String utility functions.
(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

;; Smart completion.
(ido-mode t)

;; Init flycheck
(declare-function global-flycheck-mode "flycheck.el")
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Make "home" work like in most IDEs.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

   Move point to the first non-whitespace character on this line.
   If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Indent the whole buffer.
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Smart indent region.
(defun smart-indent-region ()
  "If the mark is active, indents region.
   Otherwise it indents the entire file."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-whole-buffer))
  (whitespace-cleanup))

;; Rebind over indent-region.
(global-set-key (kbd "C-M-\\") 'smart-indent-region)

(defun long-enough-p ()
  "Is this word long enough to expand?"
  (let ((old-point (point)))
    (save-excursion
      (backward-word)
      (> (- old-point (point)) 2))))

(defun override-p ()
  "Did we manually override the length limit?"
  (eq this-command last-command))

(defvar smart-tab-always-indent nil)
(setq-local smart-tab-always-indent nil)

;; Modified from http://emacswiki.org/emacs/TabCompletion
(defun smart-tab ()
  "Smart tab does the following:
   In the mini-buffer: expand the word.

   If mark is active: indent the region.

   If we are at the end of the word or call twice: expand the word.

   Otherwise: indent the current line."
  (interactive)
  ;; Always expend in the buffer.
  (if (minibufferp)
      (dabbrev-expand nil)
    ;; Always indent a selection.
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      ;; Are we at the end of a word?
      (if (looking-at "\\_>")
          (progn
            (if smart-tab-always-indent
                (indent-for-tab-command) nil)
            (if (long-enough-p)
                (dabbrev-expand nil)
              (if (override-p)
                  (progn
                    (dabbrev--reset-global-variables)
                    (dabbrev-expand nil)
                    (indent-for-tab-command)))))
        (indent-for-tab-command)))))
;; Ignore buffers that begin with spaces.
(setq dabbrev-ignored-buffer-regexps
      (cons "^ " dabbrev-ignored-buffer-regexps))
;; Replace dabbrev-expand with hippie-expand.
(global-set-key (kbd "M-/") 'hippie-expand)
;; Bind to tab.
(global-set-key (kbd "C-i") 'smart-tab)
;; Function to enable autosave.
;; From: http://emacswiki.org/emacs/AutoSave
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file."
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-some-buffers t)))

;; Rectangles :)
(cua-selection-mode t)
;; (kbd "C-j") is Ctrl + Enter
(global-set-key (kbd "C-j") 'cua-set-rectangle-mark)

;; Revert buffer
(defun do-revert ()
  "Revert the current buffer without asking."
  (interactive)
  (revert-buffer t t))

(defun indentation-column ()
  "Returns the column that this line is indented to."
  (save-excursion
    (back-to-indentation)
    (current-column)))

;; From here: https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
(defun current-line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defvar mark-block-indent t)
(setq-local mark-block-indent t)
(defun mark-block ()
  "Put the point at the beginning of this block, mark at the end.
   The block marked is the one that contains point or follows point"
  (interactive)

  (if mark-block-indent (indent-for-tab-command))
  (if (= (indentation-column) 0)
      (mark-paragraph)
    (let* ((depth (indentation-column)))
      ;; Find end.
      (while
          (and
           (= (indentation-column) depth)
           (not (current-line-empty-p)))
        (forward-line 1)
        (if mark-block-indent (indent-for-tab-command)))
      
      (beginning-of-line)
      (set-mark (point))
      (forward-line -1)
      ;; Find beginning.
      (while (= (indentation-column) depth)
        (forward-line -1)
        (if mark-block-indent (indent-for-tab-command)))
      (forward-line 1)
      (beginning-of-line))))

(defun smart-sort-lines ()
  "If the mark is active, sorts region.
   Otherwise it sorts the current block."
  (interactive)

  (save-excursion
    (unless mark-active (mark-block))

    (sort-lines nil (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (untabify (region-beginning) (region-end))))

(global-set-key (kbd "C-c r") 'do-revert)
(global-set-key (kbd "C-c C-r") 'do-revert)
(global-set-key (kbd "C-c #") 'smart-sort-lines)
(global-set-key (kbd "M-#") 'smart-sort-lines)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

(defun proj-root ()
  (let ((res (shell-command-to-string
              "git rev-parse --show-toplevel 2>/dev/null")))
    (if (> (length res) 0)
        (substring res 0 -1)
      ".")))

(defun save-all-files ()
  "Save all modified files."
  (interactive)
  (save-some-buffers t))

(global-set-key (kbd "C-x s") 'save-all-files)

;; Setup tags.
(let ((tags-file  (concat (proj-root) "/.tags")))
  (if (file-exists-p tags-file)
      (setq tags-file-name tags-file)))

(require 'etags)
(setq tags-revert-without-query 1)

(defun rebuild-tags ()
  (start-process "rebuild-tags" nil "rebuild-tags"))

(defun end-of-line-p ()
  (let ((p (point)))
    (save-excursion
      (end-of-visible-line)
      (= p (point)))))

;; From here: https://www.emacswiki.org/emacs/ModeCompile
;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't
    ;; go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer
                                       "*compilation*"))))
  ;; Always return the anticipated result of
  ;; compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(defvar compilation-exit-message-function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Wrap parameter lists.
(defvar wrap-parameter)
;; Move to the start of the parameter list.
(defvar start-of-parameter-list)
;; Move to the end of the parameter list.
(defvar end-of-parameter-list)

(defun wrap-until (limit max-loops)
  (when (and (< 0 max-loops) (< (point) limit))
    (progn
      (funcall wrap-parameter)
      (wrap-until limit (- max-loops 1)))))

(defun wrap-parameter-list ()
  (interactive)
  (beginning-of-line)
  (funcall start-of-parameter-list)
  (let ((return
         (min (save-excursion
                (funcall end-of-parameter-list)
                (point))
              (point-max))))
    (wrap-until return 32)
    (funcall end-of-parameter-list)))

;; Fancy compilation behavior:
(require 'compile)

(defadvice compile-goto-error
    (after goto-error-single-window () activate)
  (delete-other-windows))

(require 'misc)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Exit without asking us about processes.
(defadvice save-buffers-kill-emacs
    (before save-buffers-kill-emacs-kill-procs () activate)
  (dolist (proc (process-list))
    (set-process-query-on-exit-flag proc nil)))

(global-company-mode)
(diminish 'company-mode)

(require 'xref)
(defun xref-goto-xref-close ()
  "Go to the xref then close the window!"
  (interactive)
  (xref-goto-xref)
  (delete-other-windows))

;; Make xref-goto-ref close window on jump.
(define-key xref--button-map (kbd "RET") 'xref-goto-xref-close)

;; Set up projectile.
(require 'projectile)
(global-set-key (kbd "<f6>") 'projectile-find-file)
(diminish 'projectile-mode)

(setq save-silently t)

;; Setup LSP
(require 'lsp)
(require 'lsp-ui)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-enable-snippet nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-eldoc-enable-hover nil)
(defvar treemacs-no-load-time-warnings t)
(global-set-key (kbd "<f1>") 'lsp-describe-thing-at-point)
