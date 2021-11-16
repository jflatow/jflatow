;; Who am I?
(setq user-full-name "Jared Flatow")
(setq user-mail-address "jared@convex.io")

;; My user directory is where packages and stuff are installed
(defun user-dir (standard-filename)
  (concat user-emacs-directory (convert-standard-filename standard-filename)))

;; Add `~/.emacs.d' and `~/.emacs.d/lisp' to the `load-path'
;;  and recompile anything in `~/.emacs.d/lisp' on launch
(let ((default-directory (user-dir "lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory default-directory 0))

;; Recompile elisp whenever I save
(require 'auto-recomp)

;; Package archives
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; My selected packages
;;  use `i x` in `list-packages' to install a new one quickly
;;   if its really good, add it here
;;  use `d x` on top of package line to delete
(setq package-selected-packages
      '(
        company        ;; complete anything
        idomenu        ;; better completions
        js2-mode       ;; enhanced `js-mode'
        magit          ;; git porecelain
        nodejs-repl    ;; js shell
        org            ;; organize yourself
        ob-http        ;; org-babel + restclient
        request        ;; normalized http request library
        restclient     ;; mind-blowing inline http requests
        skewer-mode    ;; send stuff to the browser via a server
        smex           ;; smarter `M-x`
        ))

;; Make sure selected packages are installed
(unless (boundp 'package--initialized)
  (package-initialize t))
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)


;;; Org bootstrap
;;   ^
;;    the convention is to use form feeds to separate/begin sections
;;    use `C-x [` and `C-x ]` to scroll through pages
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

;; Load babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (http . t)
   (js . t)
   (python . t)))


;;; Global minor modes

;; Apparently Ido is amazing, tips from:
;;  https://www.emacswiki.org/emacs/InteractivelyDoThings
;;  https://www.masteringemacs.org/article/introduction-to-ido-mode
;;   remember `C-f` after `C-x C-f` is good for creating files
;;    or `C-j` to select current input
;;   quickly delete files using `C-k` during search
;;    can also be used to kill buffers while searching elsewhere
;;   use `C-SPC` to restrict matches and then search them iteratively
;;   use `ido-completing-read' with "..." to borrow ido functionality
;;  should be using `imenu'
;;   use `idomenu' to jump around using `ido'
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)
(setq ido-auto-merge-delay-time 9)

;; Smex is Ido for commands
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Save my history
(savehist-mode t)

;; Highlight region and overwrite it when I type
(transient-mark-mode t)
(delete-selection-mode nil)
(show-paren-mode t)

;; Links should always be default
;;  `C-c RET' opens
;;   which you can discover via `C-h .'
(define-globalized-minor-mode global-goto-address-mode goto-address-mode
  (lambda () (goto-address-mode t)))
(global-goto-address-mode t)


;;; Preference variables, one-offs, and hooks
;;   oh my

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

;; Lines is lines
(setq truncate-partial-width-windows t)
(setq mode-require-final-newline nil)

;; Always show those ugly carriage returns
(setq inhibit-eol-conversion t)

;; Preferred indentation for switch statements in cc-mode
(c-set-offset 'case-label '+)

;; JavaScript indentation
(setq js-indent-level 2)

;; JS2 mode prefs
(setq js2-indent-level 2)
(setq js2-strict-missing-semi-warning nil)
(setq js2-strict-inconsistent-return-warning nil)

;; JS2 mode hooks, including for shell scripts
(add-to-list 'auto-mode-alist '("\\.js\\|\\.mjs\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Node args
(setq nodejs-repl-arguments '("--experimental-modules" "--experimental-repl-await"))

;; CSS mode prefs
(setq cssm-indent-level 4)
(setq cssm-mirror-mode nil)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; Skewer mode hooks
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;; Shortcut commands

;; Bash shell

(defun bash ()
  "Run a simple bash shell in an `ansi-term'."
  (interactive)
  (ansi-term "/bin/bash"))

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


;;; Shortcut keystrokes

;; Nice idea for quickly going back faster than `C-x b RET':
;;  http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Key chords could be interesting to experiment with
;;  but use an old fashioned keybinding for now XXX
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Suggested bindings from `C-h i m Magit m Getting Started`
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;;; Help

(defun jflatow-help ()
  "Sometimes you might need a refresher.

These docs are meant to be a self-documenting help
 this file is accessible via a persistent bookmark

Replication is good
 this is fundamental to emacs/lisp/fsf/gnu

Use `M-TAB` for `completion-at-point' in buffer
 in modes/points that support completion
  you might want to try `ido-at-point'

Understand the logic behind key bindings
 `C-c` commands are part of the `mode-specific-map'
 `C-h b` to peruse all the bindings
 `C-h m` to discover useful commands in current mode
 `C-h w` where-is command shows the bindings for command

Use `xref'
 `M-.` to `xref-find-definitions' a symbol
 `M-,` to `xref-pop-marker-stack' and go back to where search began

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
  `\\[org-capture]` `org-capture' with template

You can always go back to the `normal-mode'
 to reset the local variables and major mode of a buffer

TODO:
 experiment with
  tramp
  request.el
  use-package
  mail
  epa for encryption/decryption (builtin, with epg)
   https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
    just edit .gpg files
  notebook setup
   org
   ein

 learn his window setup:
  http://www.howardism.org/Technical/Emacs/new-window-manager.html
"
  (interactive)
  (describe-function 'jflatow-help))
