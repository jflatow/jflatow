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

;; Load my builtin settings
(require 'jflatow)


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
(use-package company
  :defer t)

;; Better help
;;  <PREFIX KEY> C-h lets you explore prefix keymaps
(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

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
  :defer t
  :bind (("C-x g" . 'magit-status)
         ("C-x M-g" . 'magit-dispatch)
         ("C-c g" . 'magit-file-dispatch)))

;; Git forge integration
(use-package forge
  :defer t
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
    (global-set-key (kbd "C-d TAB") 'org-switchb))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (python . t)
     (shell . t)
     (sqlite . t)
     (sql . t))))

;; orgtbl ascii plots
(use-package orgtbl-ascii-plot
  :defer t
  :after org)

;; orgtbl aggregation functions
(use-package orgtbl-aggregate
  :defer t
  :after org
  :straight (:host github :repo "tbanel/orgaggregate"))

;; org-babel + deno
(use-package ob-deno
  :after org
  :config
  ;; Load babel languages
  (org-babel-do-load-languages 'org-babel-load-languages '((deno . t)))
  ;; Formatting
  (add-to-list 'org-src-lang-modes '("deno" . typescript)))

;; org-babel + restclient
(use-package ob-http
  :after org
  :config
  ;; Load babel languages
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

;; Mind-blowing inline http requests
;;  TODO: begging for snippets
(use-package restclient
  :defer t)

;; Websocket library
(use-package websocket
  :defer t)

;; Structured editing
(use-package paredit
  :defer t)

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
(use-package elixir-mode
  :defer t)

;; Elixir environment
(use-package alchemist
  :defer t
  :after elixir-mode
  :bind (:map alchemist-mode-map
         ("M-TAB" . alchemist-company))
  :hook
  (elixir-mode . alchemist-mode)
  (alchemist-mode . company-mode))

;; Rust
(use-package rust-mode
  :defer t
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
(use-package uml-mode
  :defer t)

;; Solidity contracts
(use-package solidity-mode
  :defer t
  :config
  (setq c-basic-offset 4))

;; Go lang
(use-package go-mode
  :defer t
  :bind (:map go-mode-map
         ("C-c C-c" . go-run-buffer)
         ("C-c C-d" . godoc)
         ("C-c C-f" . gofmt)
         ("C-c 8" . godef-jump)
         ("C-u C-c 8" . godef-jump-other-window))
  :config
  (setq-default tab-width 4))

;; Some news
(use-package nnhackernews
  :defer t)

;; Chrome (experimental)
(use-package chrome
  :straight (:host github :repo "anticomputer/chrome.el")
  :defer t)

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
  :defer t
  :bind-keymap ("C-c i" . orgy-keys)
  :config
  (setq orgy-chatgpt-api-key (auth-secret :host "openai.com")))

;; vzi (experimental)
(use-package vzi
  :straight (:host github :repo "jflatow/vzi.el" :protocol ssh)
  :defer t
  :bind-keymap ("C-c v" . vzi-keys))
