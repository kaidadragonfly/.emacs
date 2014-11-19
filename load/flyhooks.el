(require 'flycheck)

(flycheck-define-checker sbt
  "Checker for compilation with SBT"
  :command ("esbt")
  :error-patterns
  ((error line-start "[error] " (file-name) ":" line ": " (message) line-end))
  :modes scala-mode)

(flycheck-add-next-checker 'sbt 'scala)

(global-set-key (kbd "<f7>") 'flycheck-previous-error)
(global-set-key (kbd "<f8>") 'flycheck-buffer)
(global-set-key (kbd "<f9>") 'flycheck-next-error)
