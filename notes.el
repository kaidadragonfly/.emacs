(defun chomp (s)
  "Remove whitespace from the end of 's'"
  (replace-regexp-in-string "\r?\n$" "" s))

(shell-command-to-string "git rev-parse --show-toplevel 2>/dev/null")
