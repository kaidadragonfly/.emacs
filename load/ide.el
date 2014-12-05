;; Features to make emacs more competitive with IDEs.
(require 'dabbrev)

;; Init flycheck
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

;; Setup tags.
(ignore-errors
  (setq tags-file-name (concat (proj-root) "/.tags")))
(add-hook
 'tags-table-mode-hook
 (lambda ()
   (auto-revert-mode t)))
(global-set-key (kbd "C-c M-,") 'tags-search)

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
;; Make dabbrev-replace case insensitive.
;; insensitive.
(setq dabbrev-case-replace nil)
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
    (let ((beg (progn (goto-char (region-beginning))
                      (line-beginning-position)))
          (end (progn (goto-char (region-end))
                      (line-end-position))))
      (sort-lines nil beg end)
      (indent-region beg end)
      (untabify beg end))))

(global-set-key (kbd "C-c r") 'do-revert)
(global-set-key (kbd "C-c C-r") 'do-revert)
(global-set-key (kbd "C-c #") 'smart-sort-lines)
(global-set-key (kbd "M-#") 'smart-sort-lines)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

(defun rebuild-tags ()
  (shell-command-to-string "rebuild-tags &"))

(defun proj-root ()
  (substring (shell-command-to-string "proj-root") 0 -1))

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
