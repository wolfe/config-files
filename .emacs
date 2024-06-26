(setq python-shell-interpreter "python3")

(defun unquote-string-region (beg end)
  "Unquote all the \" \n and \t characters in the region"
  (interactive "r")
  (save-restriction (narrow-to-region beg end)
                    (goto-char (point-min))
                    (while (re-search-forward "\\\\t" nil t) (replace-match "\t"))
                    (goto-char (point-min))
                    (while (re-search-forward "\\\\n" nil t) (replace-match "\n"))
                    (goto-char (point-min))
                    (while (re-search-forward "\\\\\"" nil t) (replace-match "\""))
                    ))


(defun refresh-emacs-libs ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; THIS METHOD NEEDS TO BE RE-LOADED AND RE-RUN AFTER CHANGES
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (interactive)
  (require 'package)

  (setq package-list '(use-package guide-key rainbow-mode spaceline auctex reftex pdf-tools helm-bibtex elfeed calfw calfw-ical calfw-org neotree elpy php-mode undo-tree smex auto-complete google-translate org-bullets org-super-agenda magit dash dired-subtree dired-rainbow ranger unfill define-word fuzzy hydra guess-language web-mode symon protobuf-mode google-c-style yaml-mode typescript-mode flymake-json python-mode flymake terraform-mode))
  (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ))

  (package-initialize)
  (package-refresh-contents)     ;; fetch the list of packages available
  (dolist (package package-list) ;; install the missing packages
    (unless (package-installed-p package)
      (package-install package))))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(global-set-key "\C-x\C-k" 'bury-buffer)
(global-set-key "\C-c\C-v" 'browse-url-of-buffer)

(add-hook 'python-mode-hook (lambda ()
                              (flymake-mode t)
                              (set-variable 'fill-column 120))) ; AnalyzeRe uses 120

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4)
                            (set-variable 'fill-column 79)))

(add-hook 'html-mode-hook (lambda()
                            (setq sgml-basic-offset 2)))

(add-hook 'protobuf-mode-hook (lambda ()
                                (setq c-basic-offset 2)))
(add-hook 'js-mode-hook (lambda()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level
                                  (if (string-match-p "package.json\\'" buffer-file-name)
                                      4
                                      2))))

(setq-default nxml-child-indent 4)

;;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4
              tab-width 4)
(setq-default js-indent-level 4)
(setq-default coffee-tab-width 4)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

(setq default-frame-alist '((background-color . "floral white")))

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq inhibit-startup-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (terraform-mode flymake-mode python-mode flymake-json typescript-mode yaml-mode google-c-style symon web-mode guess-language hydra fuzzy define-word unfill ranger dired-rainbow dired-subtree magit org-super-agenda org-bullets google-translate auto-complete smex undo-tree php-mode elpy neotree calfw-org calfw-ical calfw elfeed helm-bibtex pdf-tools auctex spaceline rainbow-mode guide-key use-package protobuf-mode groovy-mode)))
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl-lib)
(setq auto-mode-alist
      (cl-remove-if (lambda (x) (eq (cdr x) 'git-rebase-mode))
                    auto-mode-alist))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)
(add-hook 'json-mode-hook 'flymake-json-load)

(defun unescape-region (beg end)
  "Remove escape characters from strings copy/pasted from logs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (replace-string "\\n" "\n")
      (goto-char (point-min))
      (replace-string "\\t" "\t")
      (goto-char (point-min))
      (replace-string "\\\"" "\""))))
