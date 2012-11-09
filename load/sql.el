(add-hook
 'sql-mode-hook
 (lambda ()
   (local-set-key (kbd "C-i") 'indent-for-tab-command)
   (setq yank-no-indent-modes (cons major-mode yank-no-indent-modes))))
