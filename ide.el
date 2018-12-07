;; Features to make emacs more competitive with IDEs.
(require 'dabbrev)
(eval-when-compile (require 'cc-mode))

;; Handle git commits nicely.
(eval-when-compile (require 'git-commit))

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

(defun smart-sort-lines ()
  "If the mark is active, sorts region.
   Otherwise it sorts the current paragraph."
  (interactive)

  (save-excursion
    (unless mark-active (mark-paragraph))
    ;; (let ((beg (progn (goto-char (region-beginning))
    ;;                   (line-beginning-position)))
    ;;       (end (progn (goto-char (region-end))
    ;;                   (line-end-position))))

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

(defun save-all-files ()
  "Save all modified files."
  (interactive)
  (save-some-buffers t))

(global-set-key (kbd "C-x s") 'save-all-files)

;; Setup tags.
(let ((tags-file  (concat (proj-root) "/.tags")))
  (if (file-exists-p tags-file)
      (setq tags-file-name tags-file)))

(eval-when-compile (require 'etags))
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
(eval-when-compile (require 'compile))

(defadvice compile-goto-error
    (after goto-error-single-window () activate)
  (delete-other-windows))

(eval-when-compile (require 'misc))
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Exit without asking us about processes.
(defadvice save-buffers-kill-emacs
    (before save-buffers-kill-emacs-kill-procs () activate)
  (dolist (proc (process-list))
    (set-process-query-on-exit-flag proc nil)))

(global-company-mode)

(require 'xref)
(defun xref-goto-xref-close ()
  "Go to the xref then close the window!"
  (interactive)
  (xref-goto-xref)
  (delete-other-windows))

;; Make xref-goto-ref close window on jump.
(define-key xref--button-map (kbd "RET") 'xref-goto-xref-close)

;; Set up projectile.
(eval-when-compile (require 'projectile))
(global-set-key (kbd "<f6>") 'projectile-find-file)

(eval-when-compile (require 'diminish))
(diminish 'company-mode)
(diminish 'projectile-mode)
