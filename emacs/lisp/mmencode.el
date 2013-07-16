(defvar mm-decode-shell-command "~wolfe/bin/bin/mimencode -u"
  "The shell command for extracting MIME files")
(defvar mm-start "Content-Transfer-Encoding:[ \\t]*base64"
  "A regular expression to locate the beginning of a base64 MIME encode file")
(defvar mm-last-directory nil
  "Last directory used for saving an unpacked MIME file
This is reused for the default next save directory")
(defvar mm-end (concat "\\(\n\n\\)" "\\|"
                       "\\(--============_-[0-9]+\\)" "\\|"
                       "\\(------[_=]*NextPart_\\)"))
(defvar mm-delete-old-contents t
  "If non-nil, delete the MIME contents of the message, replacing it
with the filename of the saved MIME decoded message")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-match
;; (save-excursion ...)
;; (set-buffer (get-buffer-create " *temp*"))
;; (erase-buffer)

;; (defvar mm-start (concat "\\(--============_-[0-9]+==_============)"
;;                            "\\|"
;;                            "\\(------=_NextPart_[0-9A-Z\\.-]*\\)"))

;; ~guenther/ftp/rfc/rfc2045.txt
;; ~guenther/ftp/rfc/rfc2046.txt

(defvar quoted-printable "Content-Transfer-Encoding:[ \\t]*quoted-printable")
;; For quoted-printable:
;;	=$		=> strip the newline
;;	=[0-9A-F]{2}	=> character encoded as hex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mm-read-file-name (default-filename)
  (let* ((input (expand-file-name
                 (read-file-name
                  (if default-filename
                      (format "Write MIME body to file: (default %s) "
                              default-filename)
                    "Write MIME body to file: ")
                  mm-last-directory
                  default-filename)
                 mm-last-directory))
         (file default-filename)
         (dir (cond ((file-directory-p input)
                     (file-name-as-directory input))
                    ((string-equal input (file-name-as-directory input))
                     input)
                    (t (setq file (file-name-nondirectory input))
                       (file-name-directory input)))))
    (if (not (file-directory-p dir))
        (cond ((yes-or-no-p
                (concat "\"" dir
                        "\" does not exist, create it? "))
               (make-directory dir t))
              (t (error "Aborted"))))
    (setq mm-last-directory dir)
    (concat dir file)))

(defun mm-save-to-file ()
  "mm-save-to-file saves a MIME base64 attachment to a file.
More specifically, search forward from the point for indicators of
a MIME attachment, then prompt the user for a file to save the
extracted MIME contents."
  (interactive)
  (let* ((filename (buffer-substring
                    (progn (re-search-forward mm-start)
                           (re-search-forward "\n\n")
                           (re-search-backward "filename=\"")
                           (re-search-forward "filename=\"")
                           (point))
                    (progn (search-forward "\"")
                           (backward-char)
                           (point))))
         (start (progn (re-search-forward "\n\n")
                       (point)))
         (end (progn (re-search-forward mm-end)
                     (backward-char)
                     (beginning-of-line)
                     (point)))
         (file (mm-read-file-name (change-spaces-to-underscores filename)))
         (current-buff (current-buffer))
         (output-buff (save-excursion (find-file-literally file)
                                      (current-buffer)))
         (error-buff (get-clear-buffer "*mm-errors*")))
    (shell-command-on-region start
                             end
                             mm-decode-shell-command
                             output-buff
                             nil
                             error-buff)
    (or (buffer-empty-p error-buff)
        (progn
          (switch-to-buffer-other-window output-buff t)
          (switch-to-buffer-other-window error-buff t)
          (error ("Error occurred.  Aborted."))))
    (save-excursion
      (set-buffer output-buff)
      (write-file file t))
    (switch-to-buffer current-buff)
    (if mm-delete-old-contents
        (let ((buffer-was-read-only buffer-read-only))
          (toggle-read-only nil)
          (delete-region start end)
          (insert-string
           (concat "********************************************************\n"
                   "*       MIME CONTENTS EXTRACTED AND SAVED TO FILE       \n"
                   "* " file "\n"
                   "********************************************************\n"
                   ))
          (toggle-read-only buffer-was-read-only)))))
  
(defun change-spaces-to-underscores (s)
  (save-excursion
    (set-buffer (get-clear-buffer " *temp*"))
    (insert s)
    (beginning-of-buffer)
    (while (search-forward " " nil t)
      (replace-match "_" nil t))
    (buffer-string)))

(defun buffer-empty-p (buff)
  (save-excursion
    (set-buffer buff)
    (= (point-min) (point-max))))
                                 
(defun get-clear-buffer (name)
  (save-excursion
    (let ((buff (get-buffer-create name)))
      (set-buffer buff)
      (erase-buffer)
      buff)))
