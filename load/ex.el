(add-hook
 'elixir-mode-hook
 (lambda ()
   (alchemist-mode t)
   (require 'flycheck-mix)
   (flycheck-mix-setup)))
