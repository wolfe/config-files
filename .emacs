(defun initialize-emacs-libs ()
  (interactive)
  (require 'package)

                                        ; list the packages you want
  (setq package-list '(use-package guide-key rainbow-mode spaceline auctex reftex pdf-tools helm-bibtex elfeed calfw calfw-ical calfw-org neotree elpy php-mode undo-tree smex auto-complete google-translate org-bullets org-super-agenda magit dash dired-subtree dired-rainbow ranger unfill define-word fuzzy hydra guess-language web-mode symon))
                                        ; list the repositories containing them
  (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ))

                                        ; activate all the packages (in particular autoloads)
  (package-initialize)

                                        ; fetch the list of packages available 
  (package-refresh-contents)
  (unless package-archive-contents
    (package-refresh-contents))

                                        ; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(global-set-key "\C-x\C-k" 'bury-buffer)
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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq inhibit-startup-screen t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (groovy-mode)))
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
