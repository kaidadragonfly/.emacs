(add-hook
 'sql-mode-hook
 (lambda ()
   (local-set-key (kbd "C-i") 'indent-for-tab-command)))

(eval-after-load "sql"
  (load-library "sql-indent"))
