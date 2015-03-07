;; Local lisp
(setq my-lisp-dir "~/.emacs.d/lisp")
(let ((default-directory my-lisp-dir))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory default-directory 0))

;; Package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
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

;; CSS prefs
(setq cssm-indent-level 4)
(setq cssm-mirror-mode nil)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Objective-C style preferences (objc mode is kind of broken)
(defconst objc-style
  '((c-echo-syntactic-information-p . t)
    (c-offsets-alist . ((objc-method-args-cont . c-lineup-ObjC-method-args)
                        (objc-method-call-cont . c-lineup-ObjC-method-args))))
  "Objective-C Programming Style")

(defun objc-style-hook ()
  (c-add-style "objc" objc-style t)
  (c-set-style "objc")
  (setq tab-width 4))

(add-hook 'objc-mode-hook 'objc-style-hook)

;; Bash shell
(defun bash ()
  (interactive)
  (ansi-term "/bin/bash"))

;; Python shell
(defun py-shell ()
  (interactive)
  (run-python "/usr/bin/env python" nil 0))
(setq python-shell-setup-codes nil)

;; Erlang mode
(require 'erlang-start)

;; CRM mode
(autoload 'crm114-mode "crm114-mode" "Major mode for CRM files" t)
(add-to-list 'auto-mode-alist '("\\.crm\\'" . crm114-mode))

;; N3 mode
(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)
(add-hook 'n3-mode-hook 'turn-on-font-lock)
(setq auto-mode-alist
      (append '(("\\.n3\\'" . n3-mode)
                ("\\.owl\\'" . n3-mode)) auto-mode-alist))

;; YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for YAML files" t)
(setq auto-mode-alist
      (append '(("\\.yml\\'" . yaml-mode)
                ("\\.yaml\\'" . yaml-mode)) auto-mode-alist))

;; Go mode
(autoload 'go-mode "go-mode" "Major mode for the Go programming language" t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
