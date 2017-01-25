(defvar yaml-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?- "_" syntax-table)
    (modify-syntax-entry ?_ "_" syntax-table)
    (modify-syntax-entry ?\( "." syntax-table)
    (modify-syntax-entry ?\) "." syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table)
    (modify-syntax-entry ?\} "){" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)
    syntax-table))

(add-hook
 'yaml-mode-hook
 (lambda ()
   (auto-fill-mode 0)))
