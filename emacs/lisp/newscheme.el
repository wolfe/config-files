;; Scheme mode, and its idiosyncratic commands.
;; Copyright (C) 1985 Bill Rozas & Richard M. Stallman

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Initially a query replace of Lisp mode, except for the indentation 
;; of special forms.  Probably the code should be merged at some point 
;; so that there is sharing between both libraries.

;; Indiana University pub version by Chris Haynes -- May, 87
;; Indentation modified by Chris Haynes -- Aug, 88 and July 92
;;   set variable scheme-max-indent to maximum indentation (default 7)
;;   set variable scheme-standard-indent to standard indentation (default 2)

; (provide 'scheme)

; (defvar scheme-mode-syntax-table nil "")
; (defvar scheme-mode-abbrev-table nil "")
; (defvar scheme-max-indent 7 "")
; (defvar scheme-standard-indent 2 "")

; (if (not scheme-mode-syntax-table)
;     (let ((i 0))
;       (setq scheme-mode-syntax-table (make-syntax-table))
;       (set-syntax-table scheme-mode-syntax-table)
;       (while (< i ?0)
; 	(modify-syntax-entry i "_   ")
; 	(setq i (1+ i)))
;       (setq i (1+ ?9))
;       (while (< i ?A)
; 	(modify-syntax-entry i "_   ")
; 	(setq i (1+ i)))
;       (setq i (1+ ?Z))
;       (while (< i ?a)
; 	(modify-syntax-entry i "_   ")
; 	(setq i (1+ i)))
;       (setq i (1+ ?z))
;       (while (< i 128)
; 	(modify-syntax-entry i "_   ")
; 	(setq i (1+ i)))
;       (modify-syntax-entry ?  "    ")
;       (modify-syntax-entry ?- "_   ")
;       (modify-syntax-entry ?\t "    ")
;       (modify-syntax-entry ?\n ">   ")
;       (modify-syntax-entry ?\f ">   ")
;       (modify-syntax-entry ?\; "<   ")
;       (modify-syntax-entry ?` "'   ")
;       (modify-syntax-entry ?' "'   ")
;       (modify-syntax-entry ?, "'   ")
;       (modify-syntax-entry ?. "_   ")
;       (modify-syntax-entry ?# "'   ")
;       (modify-syntax-entry ?\" "\"    ")
;       (modify-syntax-entry ?\\ "\\   ")
;       (modify-syntax-entry ?\[ "(]  ")
;       (modify-syntax-entry ?\] ")[  ")
;       (modify-syntax-entry ?\( "()  ")
;       (modify-syntax-entry ?\) ")(  ")))

; (define-abbrev-table 'scheme-mode-abbrev-table ())

; (defun scheme-mode-variables ()
;   (set-syntax-table scheme-mode-syntax-table)
;   (setq local-abbrev-table scheme-mode-abbrev-table)
;   (make-local-variable 'indent-line-function)
;   (setq indent-line-function 'scheme-indent-line)
;   (make-local-variable 'comment-start)
;   (setq comment-start ";")
;   (make-local-variable 'comment-start-skip)
;   (setq comment-start-skip ";+ *")
;   (make-local-variable 'comment-column)
;   (setq comment-column 40)
;   (make-local-variable 'comment-indent-hook)
;   (setq comment-indent-hook 'scheme-comment-indent))

; (defun scheme-mode-commands (map)
;   (define-key map "\C-c\C-i" 'scheme-indent-definition)
;   (define-key map "\C-c\C-s" 'find-scheme-definition)
;   (define-key map "\C-c\C-b" 'both-out)
;   (define-key map "\C-cb" 'scheme-zap-buffer)
;   (define-key map "\C-cd" 'scheme-zap-definition)
;   (define-key map "\C-ce" 'scheme-zap-expression)
;   (define-key map "\C-ci" 'scheme-indent-sexp)
;   (define-key map "\C-cr" 'scheme-zap-region)
;   (define-key map "\C-h" 'backward-delete-char-untabify)
;   (define-key map "\e\C-x" 'scheme-zap-definition-and-go)
;   (define-key map "\e\C-z" 'goto-scheme)
;   (define-key map "\n" 'newline)
;   (define-key map "\r" 'newline-and-indent)
;   (define-key map "\t" 'scheme-indent-line)
;   )
; 
; (defvar scheme-mode-map (make-sparse-keymap))
; (scheme-mode-commands scheme-mode-map)

; (defun scheme-mode ()
;   "Major mode for editing Scheme code.
; Commands:
; \\{scheme-mode-map}

; Return automatically indents, while newline (C-j) does a simple return.
; C-h converts tabs to spaces as it moves back.
; Tab indents for Scheme; with argument, shifts rest
;  of expression rigidly with the current line.
; C-c i does Tab on each line starting within following expression,
; and C-c C-i similarly reindents the current definition.

; C-h converts tabs to spaces as it moves back.  Semicolons start comments.
; Entry to this mode calls the value of scheme-mode-hook 
; if that value is non-nil."
;   (interactive)
;   (kill-all-local-variables)
;   (make-local-variable 'fix-mismatch)
;   (setq fix-mismatch t)
;   (use-local-map scheme-mode-map)
;   (setq major-mode 'scheme-mode)
;   (setq mode-name "Scheme")
;   (scheme-mode-variables)
;   (run-hooks 'scheme-mode-hook))
; 
; (defun scheme-comment-indent (&optional pos)
;   (save-excursion
;     (if pos (goto-char pos))
;     (if (looking-at ";;;")
; 	(current-column)
;       (if (looking-at ";;")
; 	  (let ((tem (calculate-scheme-indent)))
; 	    (if (listp tem) (car tem) tem))
; 	(current-column)))))

; (defvar scheme-indent-offset nil "")

; (defun scheme-indent-line (&optional whole-exp)
;   "Indent current line as Scheme code.
; With argument, indent any additional lines of the same expression
; rigidly along with this one."
;   (interactive "P")
;   (let ((indent (calculate-scheme-indent)) shift-amt beg end
; 	(pos (- (point-max) (point))))
;     (beginning-of-line)
;     (setq beg (point))
;     (skip-chars-forward " \t")
;     (if (looking-at "[ \t]*;;;")
; 	;; Don't alter indentation of a ;;; comment line.
; 	nil
;       (if (listp indent) (setq indent (car indent)))
;       (setq shift-amt (- indent (current-column)))
;       (if (zerop shift-amt)
; 	  nil
; 	(delete-region beg (point))
; 	(indent-to indent))
;       ;; If initial point was within line's indentation,
;       ;; position after the indentation.  Else stay at same point in text.
;       (if (> (- (point-max) pos) (point))
; 	  (goto-char (- (point-max) pos)))
;       ;; If desired, shift remaining lines of expression the same amount.
;       (and whole-exp (not (zerop shift-amt))
; 	   (save-excursion
; 	     (goto-char beg)
; 	     (forward-sexp 1)
; 	     (setq end (point))
; 	     (goto-char beg)
; 	     (forward-line 1)
; 	     (setq beg (point))
; 	     (> end beg))
; 	   (indent-code-rigidly beg end shift-amt)))))

; (defun calculate-scheme-indent (&optional parse-start)
;   "Return appropriate indentation for current line as scheme code.
; In usual case returns an integer: the column to indent to.
; Can instead return a list, whose car is the column to indent to.
; This means that following lines at the same level of indentation
; should not necessarily be indented the same way.
; The second element of the list is the buffer position
; of the start of the containing expression."
;   (save-excursion
;     (beginning-of-line)
;     (let ((indent-point (point)) (state '(0)) paren-depth desired-indent 
; 	  (retry t) last-sexp containing-sexp)
;       (if parse-start
; 	  (goto-char parse-start)
; 	(beginning-of-defun))
;       ;; Find outermost containing sexp
;       (while (< (point) indent-point)
; 	(setq state (parse-partial-sexp (point) indent-point 0)))
;       ;; Find innermost containing sexp
;       (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
; 	(setq retry nil)
; 	(setq last-sexp (nth 2 state))
; 	(setq containing-sexp (car (cdr state)))
; 	;; Position following last unclosed open.
; 	(goto-char (1+ containing-sexp))
; 	;; Is there a complete sexp since then?
; 	(if (and last-sexp (> last-sexp (point)))
; 	    ;; Yes, but is there a containing sexp after that?
; 	    (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
; 	      (if (setq retry (car (cdr peek))) (setq state peek))))
; 	(if (not retry)
; 	    ;; Innermost containing sexp found
; 	    (progn 
; 	      (goto-char (1+ containing-sexp))
; 	      (if (not last-sexp)
; 		  ;; indent-point immediately follows open paren.
; 		  ;; Don't call hook.
; 		  (setq desired-indent (current-column))
; 		;; Move to first sexp after containing open paren
; 		(parse-partial-sexp (point) last-sexp 0 t)
; 		(cond
; 		 ((looking-at "\\s(")
; 		  ;; Looking at a list.  Don't call hook.
; 		  (if (not (> (save-excursion (forward-line 1) (point)) 
; 			      last-sexp))
; 		      (progn (goto-char last-sexp)
; 			     (beginning-of-line)
; 			     (parse-partial-sexp (point) last-sexp 0 t)))
; 		  ;; Indent under the list or under the first sexp on the
; 		  ;; same line as last-sexp.  Note that first thing on that
; 		  ;; line has to be complete sexp since we are inside the
; 		  ;; innermost containing sexp.
; 		  (backward-prefix-chars)
; 		  (setq desired-indent (current-column)))
; 		 ((save-excursion (forward-char -1) (looking-at "\\[")))
; 		  ;; Containing sexp is bracketed, so don't do anything
; 		  ;; now, which will give 2 space indent later.
; 		 ((> (save-excursion (forward-line 1) (point))
; 		     last-sexp)
; 		  ;; Last sexp is on same line as containing sexp.
; 		  ;; It's almost certainly a function call.
; 		  (parse-partial-sexp (point) last-sexp 0 t)
; 		  (if (and (/= (point) last-sexp) 
; 			   (> (+ scheme-max-indent (point)) last-sexp)
; 			   (not (scheme-indent-hookedp)))
; 		      (progn (forward-sexp 1)
; 			     (backward-prefix-chars)
; 			     (setq desired-point (current-column)))
; 		    (backward-prefix-chars)))
;                  (t
; 		  ;; Indent beneath first sexp on same line as last-sexp.
; 		  ;; Again, it's almost certainly a function call.
; 		  (goto-char last-sexp)
; 		  (beginning-of-line)
; 		  (parse-partial-sexp (point) last-sexp 0 t)
; 		  (backward-prefix-chars)
; 		  (setq desired-indent (current-column))))))))
;       ;; Point is at the point to indent under unless we are inside a string.
;       ;; Call indentation hook except when overriden by scheme-indent-offset
;       ;; or if the desired indentation has already been computed.
;       (cond ((= paren-depth 0) (setq desired-indent (current-column)))
; 	    ((car (nthcdr 3 state))
; 	     ;; Inside a string, don't change indentation.
; 	     (goto-char indent-point)
; 	     (skip-chars-forward " \t")
; 	     (setq desired-indent (current-column)))
; 	    ((and (integerp scheme-indent-offset) containing-sexp)
; 	     ;; Indent by constant offset
; 	     (goto-char containing-sexp)
; 	     (setq desired-indent (+ scheme-indent-offset (current-column))))
; 	    ((not desired-indent)
; 	     ;; Use default indentation if not computed yet
; 	     (setq desired-indent (+ (- scheme-standard-indent 1)
; 				     (current-column)))))
;       desired-indent)))

; (defun scheme-indent-hookedp ()
;   "True if point is at beginnning of special form with 
;    scheme-indent-hook property."
;   (save-excursion
;     (let ((name (buffer-substring (point);(progn (forward-char -1) (point))
; 				  (progn (forward-sexp 1) (point)))))
;       (get (intern-soft (downcase name)) 'scheme-indent-hook))))

; (put 'with 'scheme-indent-hook t)
; (put 'when 'scheme-indent-hook t)
; (put 'set! 'scheme-indent-hook t)
; (put 'let 'scheme-indent-hook t)
; (put 'let* 'scheme-indent-hook t)
; (put 'recur 'scheme-indent-hook t)
; (put 'case 'scheme-indent-hook t)
; (put 'rec 'scheme-indent-hook t)

; (defun scheme-indent-definition ()
;   "Fix indentation of the current definition."
;   (interactive)
;   (save-excursion
;     (beginning-of-defun 1)
;     (scheme-indent-sexp)))

; (defun scheme-indent-sexp ()
;   "Indent each line of the list starting just after point."
;   (interactive)
;   (let ((indent-stack (list nil)) (next-depth 0) bol
; 	outer-loop-done inner-loop-done state this-indent)
;     (save-excursion (forward-sexp 1))
;     (save-excursion
;       (setq outer-loop-done nil)
;       (while (not outer-loop-done)
; 	(setq last-depth next-depth
; 	      innerloop-done nil)
; 	(while (and (not innerloop-done)
; 		    (not (setq outer-loop-done (eobp))))
; 	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
; 					  nil nil state))
; 	  (setq next-depth (car state))
; 	  (if (car (nthcdr 4 state))
; 	      (progn (indent-for-comment)
; 		     (end-of-line)
; 		     (setcar (nthcdr 4 state) nil)))
; 	  (if (car (nthcdr 3 state))
; 	      (progn
; 		(forward-line 1)
; 		(setcar (nthcdr 5 state) nil))
; 	    (setq innerloop-done t)))
; 	(if (setq outer-loop-done (<= next-depth 0))
; 	    nil
; 	  (while (> last-depth next-depth)
; 	    (setq indent-stack (cdr indent-stack)
; 		  last-depth (1- last-depth)))
; 	  (while (< last-depth next-depth)
; 	    (setq indent-stack (cons nil indent-stack)
; 		  last-depth (1+ last-depth)))
; 	  (forward-line 1)
; 	  (setq bol (point))
; 	  (skip-chars-forward " \t")
; 	  (if (or (eobp) (looking-at "[;\n]"))
; 	      nil
; 	    (if (and (car indent-stack)
; 		     (>= (car indent-stack) 0))
; 		(setq this-indent (car indent-stack))
; 	      (let ((val (calculate-scheme-indent
; 			  (if (car indent-stack) (- (car indent-stack))))))
; 		(if (integerp val)
; 		    (setcar indent-stack
; 			    (setq this-indent val))
; 		  (setcar indent-stack (- (car (cdr val))))
; 		  (setq this-indent (car val)))))
; 	    (if (/= (current-column) this-indent)
; 		(progn (delete-region bol (point))
; 		       (indent-to this-indent)))))))))
; 
; (defun goto-scheme ()
;   "Pop the interactive scheme buffer in another window and go to it."
;   (interactive)
;   (switch-to-buffer-other-window "*scheme*"))

; (defun scheme-zap-region (start end)
;   "Zap region between point and mark into Scheme."
;   (interactive "r")
;   (send-region "scheme" start end)
;   (send-string "scheme" "\n"))

; (defun scheme-zap-expression (arg)
;   "Zap sexp before point into Scheme."
;   (interactive "P")
;   (scheme-zap-region
;    (let ((stab (syntax-table)))
;      (unwind-protect
; 	 (save-excursion
; 	   (set-syntax-table lisp-mode-syntax-table)
; 	   (forward-sexp -1)
; 	   (point))
;        (set-syntax-table stab)))
;    (point)))

; (defun scheme-zap-definition ()
;   "Zap current definition into Scheme."
;   (interactive)
;   (let ((stab (syntax-table)))
;     (unwind-protect
; 	(save-excursion
; 	  (set-syntax-table scheme-mode-syntax-table)
; 	  (if (not (= (point) (point-max))) (forward-char 1))
; 	  (beginning-of-defun 1)
; 	  (let ((start (point)))
; 	    (forward-sexp 1)
; 	    (scheme-zap-region start (point))))
;       (set-syntax-table stab))))

; (defun scheme-zap-buffer (arg)
;   "Zap whole buffer and goto Scheme"
;   (interactive "P")
;   (scheme-zap-region (point-min)
; 		     (point-max))
;   (goto-scheme))

; (defun scheme-zap-definition-and-go ()
;   "Zap current definition and goto Scheme"
;   (interactive)
;   (scheme-zap-definition)
;   (goto-scheme))

; (defun defining-p ()
;   (save-excursion
;     (let* ((here (point))
; 	   (name (buffer-substring (progn (backward-sexp 1) (point)) here)))
;       (beginning-of-defun 1)
;       (if (char-equal (char-after (point)) ?\()
; 	  (progn (forward-char 1)
; 		 (let ((sub (substring (next-sexp-as-string) 0 3)))
; 		   (if (or (string-equal sub "def") (string-equal sub "DEF"))
; 		       (progn (forward-sexp 1)
; 			      (forward-word 1)
; 			      (backward-word 1)
; 			      (string-equal name
; 					    (next-sexp-as-string))))))))))

; (defun find-scheme-definition (name)
;   "Find the definition of its argument in the current buffer"
;   (interactive "sFind Scheme definition of: ")
;   (beginning-of-buffer)
;   (let ((stop nil))
;     (while (not stop)
;       (search-forward name)
;       (setq stop (defining-p)))))

; (defun both-out ()
;   "Remove the opening and closing delimiters of the following list."
;   (interactive)
;   (forward-list 1)
;   (let ((end (point)))
;     (backward-list 1)
;     (delete-char 1)
;     (let ((bgn (point)))
;       (goto-char (- end 1))
;       (delete-backward-char 1)
;       (goto-char bgn))))

