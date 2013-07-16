(load "shell")
(defun shell (&optional shell-number)
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in shell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See shell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Note that many people's .cshrc files unconditionally clear the prompt.
If yours does, you will probably want to change it.

A negative argument causes further shell windows (*shell<2>*,
*shell<3>, etc.) to be created.  A positive argument causes the shell
window with that number to be selected, instead of *shell* (which is
number 1).  Shells with numbers > 1 must be created explicitly; shell
1 is created automatically if it is selected but does not already
exist."
  (interactive "p")
  (let* ((prog (or explicit-shell-file-name
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog))
	 (dont nil)
	 shell-name)
    ; process the user argument
    (cond
     ; a null argument is the same as 1
     ((null shell-number)
      (setq shell-number 1))
     ; a negative argument means find a new shell number
     ((< shell-number 1)
      (progn
	(setq shell-number 2)
	(while
	    (get-buffer (concat "*shell<" (int-to-string shell-number) ">*"))
	  (setq shell-number (1+ shell-number)))))
     ; if he's trying to select a shell > 1, check to see if it exists first
     ((> shell-number 1)
      (if (null (get-buffer (concat "*shell<" 
				    (int-to-string shell-number)
				    ">*")))
	  (progn
	    (ding)
	    (setq dont t)))))
    ; if we haven't found an error, do it
    (if (not dont)
	(progn
	  (setq shell-name (if (eq shell-number 1)
			       "shell"
			       (concat "shell<" 
				       (int-to-string shell-number)
				       ">")))
	  (switch-to-buffer
	   (apply 'make-shell shell-name prog
		  (if (file-exists-p (concat "~/.emacs_" name))
		      (concat "~/.emacs_" name))
		  (let ((symbol (intern-soft
				 (concat "explicit-" name "-args"))))
		    (if (and symbol (boundp symbol))
			(symbol-value symbol)
		      '("-i")))))))))
