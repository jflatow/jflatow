;; Local lisp
(setq my-lisp-dir "~/.emacs.d/lisp")
(let ((default-directory my-lisp-dir))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory default-directory 0))

;; Package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Org boostrap
(setq org-startup-folded t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-return-follows-link t)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
         "* TODO %?\n" :clock-in t :clock-resume t)))
(setq org-default-notes-file "~/Dropbox/Notes/TODO.org")
(setq org-agenda-files (list org-default-notes-file))
(when (file-exists-p org-default-notes-file)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link))

;; No splash
(setq inhibit-splash-screen t)

;; Highlight region and overwrite it when I type
(transient-mark-mode t)
(delete-selection-mode nil)
(show-paren-mode t)

;; Show column numbers
(setq column-number-mode t)

;; Lines is lines
(setq truncate-partial-width-windows t)
(setq mode-require-final-newline nil)

;; Always show those ugly carriage returns
(setq inhibit-eol-conversion t)

;; Always show that ugly trailing whitespace
(setq-default show-trailing-whitespace t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Preferred indentation for switch statements in cc-mode
(c-set-offset 'case-label '+)

;; JavaScript indentation
(setq js-indent-level 2)

;; JS2 mode prefs
(setq js2-indent-level 2)
(setq js2-strict-missing-semi-warning nil)
(setq js2-strict-inconsistent-return-warning nil)

;; CSS mode prefs
(setq cssm-indent-level 4)
(setq cssm-mirror-mode nil)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Skewer mode prefs
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Bash shell
(defun bash ()
  (interactive)
  (ansi-term "/bin/bash"))

;; Python shell
(defun py-shell ()
  (interactive)
  (run-python "/usr/bin/env python" nil 0))

;; Python shell
(defun py3-shell ()
  (interactive)
  (run-python "/usr/bin/env python3" nil 0))
