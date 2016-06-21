;;; package --- Summary
;;; Commentary:
;; This is an Emacs init for all of my programming tasks.
;; Don't expect it to fulfill any of your needs, but maybe some snips
;; from it will be useful.
;; The base requirement is package.el, which is included in the Emacs 24
;; distribution.

;;; Code:
;; no gnu splash screen on startup
(setq inhibit-splash-screen t)

;; empty message in the scratch buffer on startup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; opens a buffer on a new file without confirmation (seen in C-x C-f)
(setq confirm-nonexistent-file-or-buffer nil)

;; opens a new buffer without ido confirmation that the buffer doesn't exist.
(setq ido-create-new-buffer 'always)

;; always kill the buffer on exit
;; seems to produce the same result as
;; (setq kill-buffer-query-functions nil)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
    kill-buffer-query-functions))

;; always use spaces.
(setq-default indent-tabs-mode nil)

;; set apropos to search more broadly.
(setq apropos-do-all t)

;; don't add newline at the end of the visited file.
;; many folks won't want this.
(setq mode-require-final-newline nil)

;; Backup file buffer settings.
(setq
 backup-by-copying t                ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 0
 version-control t)                 ; use versioned backups

;; From emacs wiki. Forces a backup of the file by telling
;; emacs the file hasn't been backed up. Feels hackish.
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; No toolbar, no menu. They take up a lot of space.
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)

;; Show line number and column number in the mode line.
(line-number-mode 1)
(column-number-mode 1)

;; I always split my initial window before performing other operations,
;; so I moved it to startup.
(split-window-horizontally)

;; yes/no becomes y/n in any yes/no prompt.
(fset `yes-or-no-p 'y-or-n-p)

;; I use regex search more than search, so swap key bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; use ibuffer instead of the standard buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind these files and file types to ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; eshell customizations
;; don't leave a dead buffer when an interactive process finishes.
(setq eshell-destroy-buffer-when-process-dies t)

;; Set tramp to use ssh instead of scp
(setq tramp-default-method "ssh")

;; ediff side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; nice medium contrast theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(org-agenda-files (quote ("~/notes.org")))
 '(package-selected-packages
   (quote
    (cargo multiple-cursors go-mode ac-racer exec-path-from-shell jedi markdown-mode nodejs-repl yasnippet tern-auto-complete tern ac-js2 js2-mode yaml-mode python-django projectile flx-ido auto-complete web-mode magit rust-mode flycheck flycheck-rust virtualenvwrapper ein ess)))
 '(racer-cmd (expand-file-name "~/src/rust/racer/target/release/racer"))
 '(racer-rust-src-path (expand-file-name "~/src/rust/rust/src")))

;; org-mode experimentation. Feel free to hack this out.
(setq org-default-notes-file "~/notes.org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree "~/notes.org")
         "* %?\nEntered on %U\n %i\n %a")))
(setq org-todo-keywords '((type "TODO" "IDEA" "|" "DONE")))

;; dired-x experimentation. Feel free to hack this out.
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    (setq dired-x-hands-off-my-keys nil)
	    ))

;; Pulled from emacs wiki and modified a bit.
;; "C-c d" inserts a yyyy-mm-dd date.
(defun insert-date (prefix)
  "Insert the current org-mode formated date.
  ISO format with C-u prefix.  mm-dd-yy with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d>")
                 ((equal prefix '(4)) "%Y-%m-%d %a")
                 ((equal prefix '(16)) "%d-%m-%Y"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

;; Similar to insert-date except that we add HH:MM:SS. Adjust the format
;; strings to taste. If you don't like seconds, remove the ":%S"
(defun insert-timestamp (prefix)
  "Insert the current org-mode format timestamp.
  ISO format with C-u prefix.  common broken format with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d %H:%M:%S>")
                 ((equal prefix '(4)) "%Y-%m-%d %a %H:%M:%S")
                 ((equal prefix '(16)) "%d-%m-%Y %H:%M:%S"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c t") 'insert-timestamp)

;;
;; Try to do any work that doesn't require outside packages before this point.
;;
(require 'package)
;; extra package repositories
;;(add-to-list 'package-archives
;;             '("marmalade" .
;;               "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  ;; ths is the list of packages that we look for on startup. If some or all
  ;; are missing, we fetch and install them.
  '(
    ac-js2
    ac-racer
    auto-complete
    cargo
    ein
    ess
    exec-path-from-shell
    flx-ido
    flycheck
    flycheck-rust
    go-mode
    jedi
    js2-mode
    magit
    markdown-mode
    multiple-cursors
    nodejs-repl
    projectile
    python-django
    racer
    rust-mode
    tern
    tern-auto-complete
    virtualenvwrapper
    web-mode
    yaml-mode
    yasnippet
    )
  "Be sure these are installed at launch.")

;; cl is required for the loop
(require 'cl)
;; Checks if any packages are missing.
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

;; Install any missing packages. Updates are left as an exercise for the
;; dear reader.
(unless (prelude-packages-installed-p)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " package refresh is done")
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))
(provide 'prelude-packages)

;; It's inelegant but I want to pick up the path from shell when on
;; os x. apparently, due to a current bug there isn't a way to set
;; the path for gui apps.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; fuzzy matching in find-file, buffer searches, and other places.
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; projectile and projectile-rails settings
(projectile-global-mode)

;; turn on flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; handle mixed html / programming language files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; javascript using js2-mode and tern
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook (lambda() (tern-mode t)))
;; I have tern in a non-standard directory
(setq tern-command '("~/node/bin/node" "~/node/bin/tern"))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; if you don't want to warn on missing semicolons,
;; uncomment the following
;; (setq js2-strict-missing-semi-warning t)
;;
;; nodejs repl. You'll be shocked to know that it requires
;; the nodejs-repl package.
(global-set-key [f2] 'nodejs-repl)

;; magit is a mode for interacting with git.
;; The binding below for magit status is convenient for me.
(global-set-key (kbd "M-`") 'magit-status)

;; flycheck syntax checkers for python. flycheck will chose flake8 over pylint
;; if both are defined.
(setq flycheck-python-flake8-executable "~/.virtualenvs/emacsenv/bin/flake8")
;; (setq flycheck-python-pylint-executable "~/.virtualenvs/emacsenv/bin/pylint")
;; (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
;; jedi requires python virtualenv to run
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; python virtualenv settings
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs/")

;; ein (emacs ipython notebook) settings
(require 'ein)
(setq ein:use-auto-complete t)
(global-set-key (kbd "M-2") 'ein:notebooklist-open)

;; python django setup
(require 'python-django)
(global-set-key (kbd "M-1") 'python-django-open-project)

;; ansi-term settings
(global-set-key [f1] 'ansi-term)
;; kill process on terminal exit. scraped from echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
;; always use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; rust language setup
(defun my/racer-mode-hook ()
  (ac-racer-setup))
(add-hook 'rust-mode-hook 'my/racer-mode-hook)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(require 'yasnippet)
;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'js2-mode-hook 'yas-minor-mode)
;; no snippets for rust yet
;; (add-hook 'rust-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'ruby-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)
;;(add-to-list 'ac-modes 'python-mode)

(require 'yaml-mode)
;; yaml-mode for salt files
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
