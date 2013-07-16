;; Place the lines
;;   (setq auto-mode-alist (append '(("\\.fig$" . fig-mode)) auto-mode-alist))
;;   (autoload 'fig-mode "fig-mode" "Major mode for editing go diagrams" t ())
;; in your .emacs
;;
;; When editing file.fig, first type in your grid of X's x's O's o's
;; etc.  Then hit the backquote (`) key.  This brings up another
;; window, with all extra x's o's and other characters removed.  The
;; following keys now work to edit file.fig:
;;  (local-set-key " " 'fig-insert-coordinates)  Insert current coordinates
;;  (local-set-key "l" 'fig-insert-path)         Put in path command - then use " "
;;  (local-set-key "o" 'fig-insert-loop)         Augment last path to a loop
;;  (local-set-key "p" 'fig-insert-problem)      Problem caption
;;  (local-set-key "d" 'fig-insert-dot)          Dot
;;  (local-set-key "t" 'fig-insert-thickline)    Thick line - then use " "
;;  (local-set-key "c" 'fig-insert-caption)      Other (Solution) caption
;;  (local-set-key "e" 'fig-insert-equation)     Equation at current coordinates
;;  (local-set-key "1" 'fig-insert-equation)     Place equation off center from
;;  (local-set-key "2" 'fig-insert-equation)     current coordinates - "5" = "e"
;;  (local-set-key "3" 'fig-insert-equation)     789
;;  (local-set-key "4" 'fig-insert-equation)     456
;;  (local-set-key "5" 'fig-insert-equation)     123
;;  (local-set-key "6" 'fig-insert-equation)
;;  (local-set-key "7" 'fig-insert-equation)
;;  (local-set-key "8" 'fig-insert-equation)
;;  (local-set-key "9" 'fig-insert-equation)
;;  (local-set-key "\C-m" 'fig-insert-newline)   Put in newline
;;  (local-set-key "!" 'fig-finish)              Put in the expected gray commands
;;						 Given the line and loop commands entered
;;                                               Assumes every line is followed by loop
;;                                               using "l" and "o" commands!!!!

(defvar fig-mode-abbrev-table nil
  "Abbrev table in use in C-mode buffers.")
(define-abbrev-table 'fig-mode-abbrev-table ())
; (read-abbrev-file "~/.abbrev_defs")

(defun fig-mode () "Major mode for editing go diagrams"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fig-mode)
  (setq mode-name "GO FIGURE")
  (setq local-abbrev-table fig-mode-abbrev-table)
  (abbrev-mode 1)
  (local-set-key "`" 'fig-setup))

(defun fig-setup () "Set up a buffer with just the board"
  (interactive)
  (let ((a nil))
    (goto-char (point-min))
    (if (search-forward "\n\n" (point-max) t)
	t
      (goto-char (point-max))
      (insert "\n"))
    (forward-line -1)
    (setq a (buffer-substring (point-min) (point)))
    (goto-char (point-max))
    (delete-other-windows)
    (switch-to-buffer-other-window "*Go Setup*")
    (delete-region (point-min) (point-max))
    (kill-all-local-variables)
    (setq major-mode 'fig-setup-mode)
    (setq mode-name "Go Setup")
    (insert a)
    (setq fig-l nil)

    (setq fig-line ?A)
    (setq case-replace nil)
    (setq case-fold-search nil)
    (goto-char (point-min)) (replace-regexp "[^xXoO\.\n]" "")
    (goto-char (point-min)) (replace-regexp "[xo]" ".")
    (goto-char (point-min)) (replace-string "\n" "\n ")
    (goto-char (point-min)) (insert " ") (end-of-line)
    (let ((a (- (point) (point-min))))
      (goto-char (point-min)) (insert-char ? a) (insert "\n")
      (goto-char (point-max)) (insert-char ? (- a 1)) (insert "\n"))
    (goto-char (point-min))
    (local-set-key " " 'fig-insert-coordinates)
    (local-set-key "l" 'fig-insert-path)
    (local-set-key "o" 'fig-insert-loop)
    (local-set-key "p" 'fig-insert-problem)
    (local-set-key "d" 'fig-insert-dot)
    (local-set-key "t" 'fig-insert-thickline)
    (local-set-key "c" 'fig-insert-caption)
    (local-set-key "e" 'fig-insert-equation)
    (local-set-key "1" 'fig-insert-equation)
    (local-set-key "2" 'fig-insert-equation)
    (local-set-key "3" 'fig-insert-equation)
    (local-set-key "4" 'fig-insert-equation)
    (local-set-key "5" 'fig-insert-equation)
    (local-set-key "6" 'fig-insert-equation)
    (local-set-key "7" 'fig-insert-equation)
    (local-set-key "8" 'fig-insert-equation)
    (local-set-key "9" 'fig-insert-equation)
    (local-set-key "" 'fig-insert-newline)
    (local-set-key "!" 'fig-finish)
    (other-buffer 1) (recenter -1) (other-buffer 1)
    ))

(defun fig-insert-coordinates ()
  "Insert coordinates of current point in other buffer"
  (interactive)
  (save-excursion
    (let ((x (point)))
      (beginning-of-line)
      (setq x (concat
	       " " (int-to-string (- x (point)
				     (if (memq fig-l '(?1 ?4 ?7)) 1 0)))
	       (if (memq fig-l '(?1 ?4 ?7 ?3 ?6 ?9)) ".5" "")
	       " " (int-to-string (- (count-lines (point) (point-max))
				     (if (memq fig-l '(?1 ?2 ?3)) 2 1)))
	       (if (memq fig-l '(?1 ?2 ?3 ?7 ?8 ?9)) ".5" "")))
      (other-window 1) (insert x) (other-window 1))))

(defun fig-insert-path () "Start %PATH"
  (interactive)
  (other-window 1) (insert "\n%PATH " fig-line " ") (recenter -1) (other-window 1)
  (setq fig-line (1+ fig-line)))

(defun fig-insert-loop () "Make %LOOP identical to previous line (assumed to be %PATH)"
  (interactive)
  (other-window 1)
  (insert "\n%LOOP "
	  (save-excursion
	    (search-backward "%PATH ")
	    (forward-char 6)
	    (concat (buffer-substring (point) (progn (forward-word 1) (point)))
		    "O"
		    (buffer-substring (1+ (point)) (progn (end-of-line) (point))))))
  (recenter -1)
  (other-window 1))

(defun fig-insert-caption (caption) "%CAPTION"
  (interactive "sCAPTION: ")
  (other-window 1) (insert "%CAPTION " caption "\n") (recenter -1) (other-window 1))

(defun fig-insert-problem (problem) "%PROBLEM"
  (interactive "sPROBLEM: ")
  (other-window 1) (insert "%PROBLEM " problem "\n") (recenter -1) (other-window 1))

(defun fig-insert-equation () "%EQUATION"
  (interactive)
  (let ((l last-command-char )(eqn (read-input "EQUATION: ")))
    (other-window 1) (insert "%EQUATION") (other-window 1)
    (setq fig-l l) (fig-insert-coordinates) (setq fig-l nil)
    (other-window 1) (insert " " eqn "\n") (recenter -1) (other-window 1)))

(defun fig-insert-newline () ""
  (interactive)
  (other-window 1) (insert "\n") (recenter -1) (other-window 1))

(defun fig-insert-dot () ""
  (interactive)
  (other-window 1) (insert "\n%DOT .50") (recenter -1) (other-window 1)
  (fig-insert-coordinates))

(defun fig-insert-thickline () ""
  (interactive)
  (other-window 1) (insert "\n%THICKLINE .50 ") (recenter -1) (other-window 1))

(defun fig-finish () "Insert %GRAY and %LINE commands"
  (interactive)
  (other-window 1)
  (insert "\n\n%GRAY 1 ALL\n")
  (let ((l ?A))
    (while (< l fig-line)
      (insert "%GRAY .50 " l "O\n")
      (setq l (1+ l))))
  (insert "\n")
  (let ((l ?A))
    (while (< l fig-line)
      (insert "%LINE 1 " l "\n")
      (setq l (1+ l))))
  (recenter -1)
  (other-window 1))
