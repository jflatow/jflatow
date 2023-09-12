;;; Genesis

;; Who am I?
(setq user-full-name "Jared Flatow")
(setq user-mail-address "jared@convex.io")

;; My user directory is where packages and stuff are installed
(defun user-dir (standard-filename)
  (concat user-emacs-directory (convert-standard-filename standard-filename)))

;; Add `~/.emacs.d' and `~/.emacs.d/lisp' to the `load-path'
(let ((default-directory (user-dir "lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Recompile elisp whenever I save
(require 'auto-recomp)

;; Easy journaling
(require 'journal)


;;; Packages
;;   ^
;;    the convention is to use form feeds to separate/begin sections
;;    use `C-x [` and `C-x ]` to scroll through pages

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/jflatow/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Install use-package using straight.el by default
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Enable hiding mode line lighters
(use-package diminish)

;; Complete anything
;;  TODO: configure me?
(use-package company :defer t)

;; Better help
(use-package helpful :defer t)

;; Richer minibuffer annotations
(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; Better incremental searching
(use-package ctrlf
  :config
  (ctrlf-mode))

;; Better incremental narrowing
(use-package vertico
  :config
  (vertico-mode))

;; Predictable, efficient sorting and filtering algorithm
(use-package prescient
  :straight (:host github :repo "radian-software/prescient.el" :files ("prescient.el"))
  :config
  (prescient-persist-mode +1))

;; Connect to vertico (awkward...)
(use-package vertico-prescient
  :after vertico prescient
  :straight (:host github :repo "radian-software/prescient.el" :files ("vertico-prescient.el"))
  :config
  (vertico-prescient-mode))

;; Practical, visual commands based on `completing-read'
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c b" . consult-bookmark)
         ("C-c h" . consult-history)
         ("C-c M" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x B" . consult-buffer)
         ("C-x 4 B" . consult-buffer-other-window)
         ("C-x 5 B" . consult-buffer-other-frame)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line (also `M-g M-g`)
         ("M-g o" . consult-outline)
         ("M-g O" . consult-org-heading)
         ("M-g A" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)                  ;; TODO: use `args | pattern`
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)            ;; BUG: results can overflow minibuf
         ("M-s m" . consult-multi-occur)           ;; based on `completing-read-multiple'
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; The :init configuration is always executed (not lazy)
  :init

  ;; Improve register previews.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Other variables and modes in the :config section, after loading the package
  :config

  ;; If preview is 'any, any key triggers the preview.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Narrowing means filtering on the candidate group in consult.
  ;; Install the prefix key and enable narrowing help in the minibuffer.
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Configure project root directoryies
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project)))))

;; Virtual right-clicking
;;  more tips: https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :bind (("M-s RET" . embark-act-noquit)
         ("M-S RET" . embark-act)
         ("M-\"" . embark-dwim)
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings

  :config
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act))))

;; Embark + Consult
;;  note: this one seems problematic
;;   never got `consult-preview-at-point' working
(use-package embark-consult
  :after (embark consult))

;; Keymap reminders
(use-package which-key
  :config
  (which-key-mode)
  :diminish which-key-mode)

;; Popup management
(use-package popper
  :init
  ;; First install a keymap where `not-modified' already is
  (define-key global-map (kbd "M-~") (make-sparse-keymap))

  ;; Tell popper what to consider popup buffers by default
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          compilation-mode))

  :bind (("M-~ RET" . popper-toggle-type)
         ("M-~ `" . popper-toggle-latest)
         ("M-`" . popper-cycle))

  :config
  (popper-mode)
  (popper-echo-mode))

;; Magical git porcelain
;;  `C-c g l` to view log for active file or buffer region
;;   more tips: https://emacsredux.com/blog/2020/12/10/essential-magit-file-commands/
(use-package magit
  :bind (("C-x g" . 'magit-status)
         ("C-x M-g" . 'magit-dispatch)
         ("C-c g" . 'magit-file-dispatch)))

;; Git forge integration
(use-package forge
  :after magit)

;; Organize yourself
(use-package org
  :init
  (setq org-replace-disputed-keys t)

  :bind (:map org-mode-map
         ("C-c C-x m" . org-insert-heading-respect-content)
         ("C-c C-x M" . org-insert-todo-heading-respect-content)
         ("C-d <up>" . org-shiftup)
         ("C-d <down>" . org-shiftdown)
         ("C-d <left>" . org-shiftleft)
         ("C-d <right>" . org-shiftright))

  :config
  (defun my-org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date 1 nil))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month 1))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekyear (cond ((and (= month 1) (>= iso-week 52))
                            (1- year))
                           ((and (= month 12) (<= iso-week 1))
                            (1+ year))
                           (t year)))
           (weekstring (if (= day-of-week 1)
                           (format " W%02d" iso-week)
                         "")))
      (format "%4d %s %2d %s%s" year monthname day dayname weekstring)))

  (require 'ox-md)
  (setq org-startup-folded nil
        org-startup-indented t
        org-archive-subtree-save-file-p t
        org-hide-leading-stars t
        org-log-done 'note
        org-return-follows-link t
        org-cycle-emulate-tab nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-support-shift-select nil)
  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %T\n:END:")))
  (setq org-default-notes-file "~/Dropbox/Notes/TODO.org")
  (setq org-agenda-files (list org-default-notes-file))
  (setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
  (when (file-exists-p org-default-notes-file)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-d `") 'org-cycle-agenda-files)
    (global-set-key (kbd "C-d TAB") 'org-switchb)))

;; org ascii plots
(use-package orgtbl-ascii-plot
  :after org)

;; org-babel + deno
(use-package ob-deno
  :after org
  :config
  ;; Load babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((deno . t)))
  ;; Formatting
  (add-to-list 'org-src-lang-modes '("deno" . typescript)))

;; org-babel + restclient
(use-package ob-http
  :after org
  :config
  ;; Load babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)
     (js . t)
     (python . t)
     (shell . t))))

;; Mind-blowing inline http requests
;;  TODO: begging for snippets
(use-package restclient :defer t)

;; Websocket library
(use-package websocket :defer t)

;; Structured editing
(use-package paredit)

;; Enhanced `js-mode'
(use-package js2-mode
  :defer t
  :init
  ;; JS2 mode hooks, including for shell scripts
  (add-to-list 'auto-mode-alist '("\\.js\\|\\.mjs\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil
        js2-strict-inconsistent-return-warning nil))

;; Elixir mode
(use-package elixir-mode :defer t)

;; Elixir environment
(use-package alchemist
  :after elixir-mode
  :bind (:map alchemist-mode-map
         ("M-TAB" . alchemist-company))
  :hook
  (elixir-mode . alchemist-mode)
  (alchemist-mode . company-mode))

;; Rust
(use-package rust-mode :defer t
  :bind (:map rust-mode-map
         ("C-x C-e" . rust-run)
         ("C-x C-t" . rust-test)))

;; TypeScript
(use-package typescript-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
  :config
  (set-variable 'typescript-indent-level 2))

;; JS shell
(use-package nodejs-repl
  :defer t
  :config
  (set-variable 'nodejs-repl-arguments '("--experimental-modules" "--experimental-repl-await"))
  (set-variable 'nodejs-repl-use-global "true"))

;; Send stuff to the browser via a server
;;  TODO: latest version seems to be broken
(use-package skewer-mode
  :defer t
  :init
  (skewer-setup))

;; Sequence diagrams
(use-package uml-mode :defer t)

;; Solidity contracts
(use-package solidity-mode
  :defer t
  :config
  (setq c-basic-offset 4))

;; Go lang
(use-package go-mode
  :defer t
  :config
  (setq-default tab-width 4)
  (define-key go-mode-map (kbd "C-c C-c") 'go-run-buffer)
  (define-key go-mode-map (kbd "C-c C-d") 'godoc)
  (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
  (define-key go-mode-map (kbd "C-c 8") 'godef-jump)
  (define-key go-mode-map (kbd "C-u C-c 8") 'godef-jump-other-window))

;; Some news
(use-package nnhackernews)

;; Chrome (experimental)
(use-package chrome
  :straight (:host github :repo "anticomputer/chrome.el"))

;; ChatGPT (experimental)
(use-package chatgpt-shell
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el"))
  :defer t
  :config
  (setq chatgpt-shell-openai-key (auth-secret :host "openai.com"))
  (setq chatgpt-shell-model-version "gpt-4"))

;; orgy (experimental)
(use-package orgy
  :straight (:local-repo "~/Dropbox/Code/project/orgy")
  :bind-keymap ("C-c i" . orgy-keys)
  :config
  (setq orgy-chatgpt-api-key (auth-secret :host "openai.com")))

;; vzi (experimental)
(use-package vzi
  :straight (:host github :repo "jflatow/vzi.el" :protocol ssh)
  :bind-keymap ("C-c v" . vzi-keys))


;;; Global minor modes

(define-globalized-minor-mode global-goto-address-mode goto-address-mode
  (lambda () (goto-address-mode t)) :group 'mine)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The indicator for the mode line.
  :lighter " Sensitive"
  ;; The minor mode bindings.
  :after-hook
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(defun jflatow-minors ()
  "Set my minor modes."
  (interactive)

  ;; Save my history
  (savehist-mode t)

  ;; Highlight region and overwrite it when I type
  (transient-mark-mode t)
  (delete-selection-mode nil)
  (show-paren-mode t)

  ;; Tab bar history
  (tab-bar-history-mode t)

  ;; Winners record window configuration changes
  (winner-mode t)

  ;; Links should always be default
  ;;  `C-c RET' opens
  ;;   which you can discover via `C-h .'
  (global-goto-address-mode t)

  ;; Don't forget to mark gpg files as sensitive
  (add-to-list 'auto-mode-alist '("\\.gpg$" . sensitive-mode)))
(jflatow-minors)


;;; Random preferences for builtins

(defun jflatow-vars ()
  "Set my preference vars."
  (interactive)

  ;; No splash
  (setq inhibit-splash-screen t)

  ;; Always show that ugly trailing whitespace
  (setq-default show-trailing-whitespace t)

  ;; ABD
  (setq-default debug-on-error t)

  ;; No tabs
  (setq-default indent-tabs-mode nil)

  ;; Show column numbers
  (setq-default column-number-mode t)

  ;; Don't forget how to `widen'
  (put 'narrow-to-region 'disabled nil)

  ;; Only ever confirm with a single key
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Lines is lines
  (setq truncate-partial-width-windows t)
  (setq mode-require-final-newline nil)

  ;; Always show those ugly carriage returns
  (setq inhibit-eol-conversion t)

  ;; Save bookmarks as soon as they are created
  (setq bookmark-save-flag 1)

  ;; Preferred indentation for switch statements in cc-mode
  (c-set-offset 'case-label '+)

  ;; Tab bar preferences
  (setq tab-bar-close-button-show 'selected
        tab-bar-select-tab-modifiers '(control) ;; BUG C-<#> broken on mac
        tab-bar-tab-hints nil)

  ;; Clarify tabs a little
  (set-face-attribute 'tab-bar-tab nil ;; all tabs
                      :background "#" :foreground "black" :box nil)
  (set-face-attribute 'tab-bar-tab-inactive nil ;; inactive tabs
                      :background "gray" :foreground "black" :box nil)

  ;; Filter internet relay chat noise
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; Encryption and auth sources
  (setq auth-source-debug t)
  (setq auth-sources
        '((:source "~/.authinfo.gpg")))
  (setq epg-pinentry-mode 'loopback))
(jflatow-vars)


;;; Shortcut keystrokes

;; Nice idea for quickly going back faster than `C-x b RET':
;;  http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun jflatow-keys ()
  "Set my global keys."
  (interactive)
  ;; Re-map key translation for option-arrow keys on Mac
  (define-key input-decode-map (kbd "ESC ESC O C") (kbd "M-<right>"))
  (define-key input-decode-map (kbd "ESC ESC O D") (kbd "M-<left>"))

  ;; Key chords could be interesting to experiment with
  ;;  but use some old fashioned keybinding for now (TODO)

  ;; Multi occur by buffer pattern
  (global-set-key (kbd "C-c m") 'multi-occur-in-matching-buffers)

  ;; Instant `journal-open'
  (global-set-key (kbd "C-c j") 'journal-open)

  ;; Use helpful instead of builtin help
  ;;  <PREFIX KEY> C-h lets you explore prefix keymaps
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Install tab selection helpers
  (global-set-key (kbd "C-<left>") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "C-<right>") 'tab-bar-switch-to-next-tab)

  ;; Move the default `C-d` binding, replace it with a keymap
  (global-set-key (kbd "C-<delete>") 'delete-char)
  (define-key global-map (kbd "C-d") (make-sparse-keymap))

  ;; Install the window movement helpers
  (windmove-default-keybindings)

  ;; Convenient copying
  (global-set-key (kbd "C-c p") 'pbcopy-kill-ring-item))
(jflatow-keys)


;;; Shortcut commands

;; Auth secrets
(defun auth-secret (&rest args)
  "Retrieve the secret from the authinfo using ARGS as the search criteria."
  (let ((secret (plist-get (car (apply 'auth-source-search args)) :secret)))
    (if (functionp secret) (funcall secret) secret)))

;; Bash shell

(defun bash ()
  "Run a simple bash shell in an `ansi-term'."
  (interactive)
  (ansi-term "/bin/bash"))

;; Go

(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

;; Python and Python3 shells

(defun py-shell ()
  "Run a python shell using `run-python'

NB: this and `py3-shell' currently share the same buffer,
 could hack something else using `run-python-internal',
  and overriding anything and everything
   including `python-shell-calculate-command'"
  (interactive)
  (run-python "/usr/bin/env python" nil 0))

(defun py3-shell ()
  "Run a python3 shell using `run-python'

NB: shares buffer with `py-shell'"
  (interactive)
  (run-python "/usr/bin/env python3" nil 0))

;; Convenient pbcopy

(defun pbcopy-kill-ring-item (&optional arg)
  "Copy the last kill ring item to the clipboard using `pbcopy`.
With a prefix ARG, copy the nth previous kill ring item."
  (interactive "P")
  (let ((item (nth (or arg 0) kill-ring)))
    (when item
      (with-temp-buffer
        (insert item)
        (call-process-region (point-min) (point-max) "pbcopy"))
      (message "Copied to clipboard: %s" item))))


;;; Help

(defun jflatow-help ()
  "Sometimes you might need a refresher.

These docs are meant to be a self-documenting help
 this file is accessible via a persistent bookmark

Replication is good
 this is fundamental to emacs/lisp/fsf/gnu

Use `M-TAB` for `completion-at-point' in buffer
 in modes/points that support completion

Also, emacs has tabs and `tab-bar-mode'
 they are named (session) persistent window configurations
  `C-x t 2` new tab
  `C-x t o` other tab
  `C-x t 0` close tab

Don't forget about
 `M-A` to cycle `marginalia'
 `M-a`, `M-e` `forward-sentence' and `backward-sentence'
 `M-p`, `M-n`, `M-r` are generally useful for minibuffer history
 `M-w` even better than `copy-region-as-kill'
 `M-DEL` to `backward-kill-word'
 `M--` negate argument to invert command (often backwards)

Understand the logic behind key bindings
 `C-c` commands are part of the `mode-specific-map'
 `C-h b` to peruse all the bindings
 `C-h m` to discover useful commands in current mode
 `C-h w` where-is command shows the bindings for command

Use `xref'
 `M-.` to `xref-find-definitions' a symbol
 `M-,` to `xref-pop-marker-stack' and go back to where search began

Use bookmarks & registers, mark & point
 `C-x r C-h` for a refresher on bookmarks & registers
 `C-x r w <reg>` save window config
 `C-x r j <reg>` load window config
 `C-x C-x` to `exchange-point-and-mark' and visualize them

Use keyboard macros
 https://www.emacswiki.org/emacs/KeyboardMacros
  `\\[kmacro-start-macro-or-insert-counter]` `kmacro-start-macro-or-insert-counter'
  `\\[kmacro-end-or-call-macro]` `kmacro-end-or-call-macro'

For shells, `comint' is infinitely useful
 https://www.masteringemacs.org/article/comint-writing-command-interpreter
 https://www.masteringemacs.org/article/shell-comint-secrets-history-commands
  `C-c C-x` is amazing in shells for repeating seq of commands
  `C-u <N> C-c .` pick an arg
  `C-c C-l` list history
  `C-c M-r` history search backward
  `C-c C-r` beginning of last output
  `C-c C-o` flush last output
  `C-c C-s` save last output (as it is now)

Use `ielm' instead of *scratch* buffer for some stuff
 normal tab completion
  and you can see output inline

For some stuff, `eshell' is pretty awesome
 can just redirect to a buffer `ls >>> #<mybuf>`
  but not a good idea for data pipes
   as everything passes through interpreter
    otherwise use one of:
     `\\[shell-command]` `shell-command'
     `\\[shell-command-on-region]` `shell-command-on-region'
     `\\[shell]` `shell'

Remember comments, info, and docs
 `\\[comment-dwim]` `comment-dwim'
  `C-h r m comments RET m comment commands RET`
 doc tips in the manual:
  `C-h r s emacs lisp RET RET m documentation RET s tips RET RET`
   can use `checkdoc-minor-mode'

Org pays dividends, remember `org-info'
 capture todos
  `\\[org-capture]` `org-capture' with template
 add/remove agenda files on the fly with
  `\\[org-agenda-file-to-front]` `org-agenda-file-to-front'
  `\\[org-remove-file]` org-remove-file'
 jump around with
  `\\[org-cycle-agenda-files]` `org-cycle-agenda-files'
  `\\[org-switchb]` `org-switchb'

You can always go back to the `normal-mode'
 to reset the local variables and major mode of a buffer

To test out big changes you can create a fake user dir and do:
 `HOME=. emacs --debug-init`

There's a `speedbar'
 and appointments (i.e. `appt-activate')

TRAMP is amazing and it works out of the box
 https://www.gnu.org/software/tramp
  especially with selectrum, just start typing `C-x C-f /scp:`

`C-u C-x =` info about char under cursor

TODO:
 experiment with
  tramp over Docker
   https://willschenk.com/articles/2020/tramp_tricks/
    can be chained together with other methods

  start using snippets
   e.g. https://github.com/magnars/.emacs.d/tree/master/snippets
  bunch of cool ideas, like the dude with quick-calc wrapper
   https://www.reddit.com/r/emacs/comments/l51ocx/what_is_the_most_useful_part_of_your_emacs_config/

 learn his window setup:
  http://www.howardism.org/Technical/Emacs/new-window-manager.html

 notes on mail/gnus
 notes for epa for encryption/decryption (builtin, with epg)
  https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
   tl;dr: just edit .gpg files
    and add sensitive mode

 add function(s) to configure/connect erc
  https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
"
  (interactive)
  (describe-function 'jflatow-help))
