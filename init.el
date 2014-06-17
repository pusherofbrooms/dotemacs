;; This is an emacs init for rails. It is tested with emacs 24.3 and 24.4.
;; The base requirement is package.el, which is included in the emacs 24
;; distribution.

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

;; default is on. Change to 0 to turn it off. I like the visual queue
;;these days.
(blink-cursor-mode 1)

;; I use the scroll bar to indicate where in the file I am. Some folks
;; turn this off.
;;(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

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

;; nice medium contrast theme
(custom-set-variables
 '(custom-enabled-themes (quote (wombat)))
;; Which files org-mode agenda should track
 '(org-agenda-files (quote ("~/notes.org"))))

;; org-mode experimentation. Feel free to hack this out.
(setq org-default-notes-file "~/notes.org")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree "~/notes.org")
         "* %?\nEntered on %U\n %i\n %a")))

;; dired-x experimentation. Feel free to hack this out.
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    (setq dired-x-hands-off-my-keys nil)
	    ))

;; Pulled from emacs wiki and modified a bit.
;; "C-c d" inserts a yyyy-mm-dd date.
(defun insert-date (prefix)
  "Insert the current org-mode formated date. ISO format with
C-u prefix. mm-dd-yy with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d %a>")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d-%m-%Y"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

;; Similar to insert-date except that we add HH:MM:SS. Adjust the format
;; strings to taste. If you don't like seconds, remove the ":%S"
(defun insert-timestamp (prefix)
  "Insert the current org-mode format timestamp. ISO format with C-u
prefix. common broken format with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d %a %H:%M:%S>")
                 ((equal prefix '(4)) "%Y-%m-%d %H:%M:%S")
                 ((equal prefix '(16)) "%d-%m-%Y %H:%M:%S"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c t") 'insert-timestamp)

;;
;; Try to do any work that doesn't require outside packages before this point.
;;
(require 'package)
;; extra package repositories
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar prelude-packages
  ;; ths is the list of packages that we look for on startup. If some or all
  ;; are missing, we fetch and install them.
  '(inf-ruby rvm projectile projectile-rails flx-ido robe auto-complete
    web-mode bundler magit scss-mode)
  "Be sure these are installed at launch")

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

;; fuzzy matching in find-file, buffer searches, and other places.
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; inferior ruby mode.
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; activate rvm and use the default gem set.
(require 'rvm)
(rvm-use-default)
;; (global-set-key (kbd "C-c r a") 'rvm-activate-corresponding-ruby)

;; projectile and projectile-rails settings
(projectile-global-mode)
;; (add-hook 'ruby-mode-hook 'projectile-on)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; robe mode for code navigation
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Interact with bundle.
(require 'bundler)
(global-set-key (kbd "C-c r b i") 'bundle-install)
(global-set-key (kbd "C-c r b u") 'bundle-update)
(global-set-key (kbd "C-c r b c") 'bundle-console)

;; handle mixed html / programming language files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))

;; magit is a mode for interacting with git.
;; The binding below for magit status is convenient for me.
(global-set-key (kbd "M-`") 'magit-status)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-modes 'python-mode)
;; autocomplete for inf-ruby
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; turn off auto-compile for scss mode as it doesn't seem to work with
;; rails.
(setq scss-compile-at-save nil)

