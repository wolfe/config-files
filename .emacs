(add-to-list 'load-path "~/emacs/")

(defun random-characters (n)
  (let* ((choices "abcdefghjkmnopqrstuvwxyzABCDEFGHJKLMNQRSTUVWXYZ23456789")
         (len (length choices))
         (char (lambda (dummy) (let ((r (random len)))
                                 (substring choices r (+ r 1))))))
    (mapconcat char (make-list n 0) "")))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(setq
      auto-mode-alist (append '(("\\.ss\\'" . scheme-mode)
                                ("\\.py\\'" . python-mode)
                                ("\\.txt\\'" . fundamental-mode)
                                ("\\.mail\\'" . mail-mode)
                                ("\\.bib\\'" . bibtex-mode)
                                ("\\.yaml\\'" . fundamental-mode)
                                ("\\.less\\'" . less-mode)
                                ("\\.h\\'" . c++-mode)
                                ("\\.fig\\'" . fig-mode)
                                ("\\.g4\\'" . antlr-mode)
                                ("\\.m\\'" . matlab-mode))
                              auto-mode-alist
                              '((".*" . text-mode))))
(global-set-key "\C-x\C-k" 'bury-buffer)
(global-set-key "\C-cl" 'run-lisp)
(global-set-key "\C-cs" 'run-scheme)
(global-set-key "\C-c\C-v" 'browse-url-of-buffer)
(add-hook 'python-mode-hook (lambda ()
                              (flymake-mode t)
                              (vvb-mode t)
                              (set-variable 'fill-column 79)))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4)
                            (set-variable 'fill-column 79)))

(add-hook 'html-mode-hook (lambda()
                            (setq sgml-basic-offset 4)))

;;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4
              tab-width 4)
(setq-default js-indent-level 4)
(setq-default coffee-tab-width 4)
(add-hook 'php-mode-hook (lambda ()
                           (c-set-style "php-wolfe")))

(c-add-style "php-wolfe" ;; Type C-c C-s when a brace is on its own line
             '("bsd"
;;			   (c-hanging-braces-alist . '((defun-open after))) 
			   (c-hanging-braces-alist
				((substatement-open)
				 (block-close . c-snug-do-while)
				 (extern-lang-open after)
				 (inexpr-class-open after)
				 (defun-open after)
				 (inexpr-class-close before)))
               (c-basic-offset . 4)
			   (tab-width . 4)))

;;===== PyFlakes
;; code checking via pyflakes+flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8" (list local-file)))) ;; Need to globally pip install flake8
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode t) ;; Auto-start on any markup modes
(add-hook 'html-helper-mode-hook 'zencoding-mode t) ;; Auto-start on any markup modes

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'sendmail-send-it)
(setq mail-setup-with-from t)
(setq user-mail-address "davidgameswolfe@gmail.com")
(fset 'survey-mail
   [?\C-  escape ?f escape ?w ?\C-x ?4 ?f ?~ ?/ ?t ?m ?p ?/ ?a ?. ?m ?a ?i ?l return escape ?> ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-e ?\C-y escape ?< ?\C-e ?\C-x ?o ?\C-f ?\C-  ?\C-e escape ?w ?\C-f ?\C-x ?o ?\C-y ?\C-c ?\C-c ?y ?\C-x ?b ?a ?m backspace ?. ?m ?a ?\C-i return ?\C-x ?k return ?y ?\C-x ?o])

(add-to-list 'load-path "~/emacs/coffee-mode")
(require 'coffee-mode)
(add-to-list 'load-path "~/emacs/less-mode")
(require 'less-mode)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mdl" . fundamental-mode) auto-mode-alist))

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/folder-where-you-put-scss-mode-el"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq-default scss-compile-at-save nil)

(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)
(add-to-list 'auto-mode-alist '("\\.tpl" . smarty-mode))

(fset 'extract-test-names
   [?\C-s ?: ?  ?\C-f ?\C-b ?\C-  ?\C-a ?\C-w ?\C-s ?  ?\C-b ?\C-d ?\C-d ?\C-x ?\C-x ?\C-w ?\C-s ?. ?\C-f ?\C-b ?\C-  ?\C-e ?\C-r ?. ?\C-b ?\C-f ?\C-f ?\C-w ?\C-e backspace ?\C-y escape ?y ?\C-x ?\C-x ?\C-b ?\C-f ?. ?\C-e ?  ?\\ ?\C-n ?\C-a])

(setq default-frame-alist '((background-color . "floral white")))

(require 'python)
; indentation
(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
  they line up with the line containing the corresponding opening bracket."
(save-excursion
  (beginning-of-line)
  (let ((syntax (syntax-ppss)))
    (if (and (not (eq 'string (syntax-ppss-context syntax)))
             (python-continuation-line-p)
             (cadr syntax)
             (skip-syntax-forward "-")
             (looking-at "\\s)"))
        (progn
          (forward-char 1)
          (ignore-errors (backward-sexp))
          (setq ad-return-value (current-indentation)))
      ad-do-it))))

(ad-activate 'python-calculate-indentation)

(defun shuffle-lines-region (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'google-set-c-style "google-c-style")
(autoload 'column-marker-1 "column-marker")
(add-hook 'c-mode-common-hook (lambda ()
                                (subword-mode)
                                (google-set-c-style)
                                (set-fill-column 100)
                                (column-marker-1 100)
                                (setq c-basic-offset 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; QRA
(add-to-list 'load-path "~/emacs/matlab-emacs-src")
;; (load-library "matlab-load")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq inhibit-startup-screen t)
