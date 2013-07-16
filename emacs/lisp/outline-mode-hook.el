(defun outline-mode-hook-file () ""
  (local-set-key "\t" '(lambda () "insert five spaces" (interactive)
			 (insert "     ")))
  (local-set-key "\C-cs" 'show-entry)
  (local-set-key "\C-ch" 'hide-entry))
