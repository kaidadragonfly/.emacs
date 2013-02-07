;; Features to make emacs more competitive with IDEs.

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
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

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
(global-set-key "\C-\M-\\" 'smart-indent-region)

(defun long-enough-p ()
  (> (- (point) (car (bounds-of-thing-at-point 'word))) 2))

;; Modified from http://emacswiki.org/emacs/TabCompletion
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (indent-for-tab-command)
  (if (minibufferp)
      (dabbrev-expand nil)
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (and (looking-at "\\_>") (long-enough-p))
          (dabbrev-expand nil)))))
;; Make dabbrev-replace case insensitive.
(setq dabbrev-case-replace nil)
;; Replace dabbrev-expand with hippie-expand.
(global-set-key (kbd "M-/") 'hippie-expand)
;; Bind to tab.
(global-set-key (kbd "C-i") 'smart-tab)
;; Function to enable autosave.
;; From: http://emacswiki.org/emacs/AutoSave
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (and (buffer-file-name) (buffer-modified-p))
      (save-some-buffers t)))
;; Add an auto save hook to enable.
;; (add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;; Make yank indent.
;; Modified from: http://emacswiki.org/emacs/AutoIndentation
(setq yank-no-indent-modes '())

;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (not current-prefix-arg)
;;            (let
;;                ((mark-even-if-inactive transient-mark-mode))
;;              (if (not (member major-mode yank-no-indent-modes))
;;                  (indent-region
;;                   (region-beginning) (region-end) nil))))))

;; Rectangles :)
(cua-selection-mode t)
(global-set-key "\C-j" 'cua-set-rectangle-mark)
