(add-hook
 'sql-mode-hook
 (lambda ()
   (local-set-key (kbd "C-i") 'indent-for-tab-command)))
