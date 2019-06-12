(add-hook
 'smerge-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c u") 'smerge-keep-upper)
   (local-set-key (kbd "C-c l") 'smerge-keep-lower)
   (local-set-key (kbd "C-c n") 'smerge-next)
   (local-set-key (kbd "C-c p") 'smerge-prev)))
