;; Make flymake work with the console.  
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in
the minibuffer.  "
  (interactive)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))

(add-hook 
 'flymake-mode-hook 
 (lambda ()
   ;; Enable showing flymake errors.
   (set (make-local-variable 'post-command-hook)
	(cons 'show-fly-err-at-point post-command-hook))
   ;; Setup keybindings.  
   (local-set-key (kbd "<f7>") 'flymake-goto-prev-error)
   (local-set-key (kbd "<f8>") 'flymake-goto-next-error)))
