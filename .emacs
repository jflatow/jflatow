(setq load-path (cons "~/.emacs.d/lisp" load-path))
(setq inhibit-splash-screen t)

;; add all the subdirectories to the load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Highlight region and overwrite it when I type
(transient-mark-mode t)
(delete-selection-mode nil)
(show-paren-mode t)

;; Show column numbers
(setq column-number-mode t)

;; Lines is lines
(setq truncate-partial-width-windows t)

;; Always show those ugly carriage returns
(setq inhibit-eol-conversion t)

;; Always show that ugly trailing whitespace
(setq-default show-trailing-whitespace t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Preferred indentation for switch statements in cc-mode
(c-set-offset 'case-label '+)

;; bash shell
(defun bash ()
  (interactive)
  (ansi-term "/bin/bash"))

;; Use a real man's python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'py-shell "python-mode" "Python shell." t nil)

(defun py (&optional argprompt)
  "Start a python shell using the given command."
  (interactive "sPython command: ")
  (when argprompt
    (custom-set-variables
     '(py-which-shell argprompt)
     '(py-python-command argprompt)))
  (py-shell))

;;;;;;;;;;;;;;;
;; Emacs hints from Georg Brandl (for core Python developers)
;; highlight XXX style code tags in source files
(font-lock-add-keywords 'python-mode
			'(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\)" 1 font-lock-warning-face prepend)))

;; good for defeating the whitespace-normalization commit hook
;(setq-default show-trailing-whitespace t)
;;;;;;;;;;;;;;;


;; Fix Java mode (annotations, indentations)
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook
	  'java-mode-indent-annotations-setup)

(defun my-java-mode-hook ()
  (setq c-basic-offset 2))
(add-hook 'java-mode-hook 'my-java-mode-hook)


;; Use sgml-mode for jsp pages and tag files
(add-to-list 'auto-mode-alist '("\.jsp$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\.tag$" . sgml-mode))

;; ReStructured Text
(require 'rst)
(setq rst-mode-lazy nil)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.txt$" . rst-mode)
                ("\\.rest$" . rst-mode))
              auto-mode-alist))

;; CRM mode
(autoload 'crm114-mode "crm114-mode" "Major mode for CRM files" t)
(add-to-list 'auto-mode-alist '("\\.crm\\'" . crm114-mode))

;; CSS Mode
(require 'css-mode)
(add-to-list 'auto-mode-alist '("\.css$" . css-mode))
(setq cssm-indent-level 4)
(setq cssm-mirror-mode nil)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; N3 Mode
(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)

;; Turn on font lock when in n3 mode
(add-hook 'n3-mode-hook 'turn-on-font-lock)

(setq auto-mode-alist
      (append '(("\\.n3" . n3-mode)
                ("\\.owl" . n3-mode)) auto-mode-alist))

;; YAML Mode
(autoload 'yaml-mode "yaml-mode" "Major mode for YAML files" t)

(setq auto-mode-alist
      (append '(("\\.yml" . yaml-mode)
                ("\\.yaml" . yaml-mode)) auto-mode-alist))

;; nav bar
(require 'nav)

;; Objective-C style preferences (objc mode is kind of broken)
(defconst objc-style
  '((c-echo-syntactic-information-p . t)
    (c-offsets-alist .
                     ((objc-method-args-cont . c-lineup-ObjC-method-args)
                      (objc-method-call-cont . c-lineup-ObjC-method-args))
                     ))
  "Objective-C Programming Style")

(defun objc-style-hook ()
  (c-add-style "objc" objc-style t)
  (c-set-style "objc")
  (setq tab-width 4))

(add-hook 'objc-mode-hook 'objc-style-hook)

;; erlang mode
(require 'erlang-start)

;; ECMAScript mode
(require 'ecmascript-mode)
(setq auto-mode-alist
      (append '(("\\.as$" . ecmascript-mode)
                ("\\.js$" . ecmascript-mode))
              auto-mode-alist))

;; go mode
(require 'go-mode-load)

;; haskell mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
