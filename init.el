
(setq inhibit-splash-screen t)
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;;(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(line-number-mode 1)
(column-number-mode 1)

;; No confirm messages
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
;; always kill the buffer on exit
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
    kill-buffer-query-functions))
;; yes/no becomes y/n
(fset `yes-or-no-p 'y-or-n-p)

;; Custom 'apropos' key bindings
(global-set-key (kbd "C-h C-a") 'Apropos-Prefix)
(define-prefix-command 'Apropos-Prefix nil "Apropos (a,d,f,i,l,v,C-v)")
(define-key Apropos-Prefix (kbd "a")   'apropos)
(define-key Apropos-Prefix (kbd "C-a") 'apropos)
(define-key Apropos-Prefix (kbd "d")   'apropos-documentation)
(define-key Apropos-Prefix (kbd "f")   'apropos-command)
(define-key Apropos-Prefix (kbd "c")   'apropos-command)
(define-key Apropos-Prefix (kbd "i")   'info-apropos)
(define-key Apropos-Prefix (kbd "l")   'apropos-library)
(define-key Apropos-Prefix (kbd "v")   'apropos-variable)
(define-key Apropos-Prefix (kbd "C-v") 'apropos-value)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; nice medium contrast theme
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))

(setq org-default-notes-file "~/notes.org")
(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    (setq dired-x-hands-off-my-keys nil)
	    ))


;;
;; Try to do any work that doesn't require outside packages before this
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
  '(inf-ruby rvm projectile projectile-rails flx-ido robe auto-complete
    web-mode bundler magit scss-mode)
  "Be sure these are installed at launch")

;; cl is required for the loop
(require 'cl)
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (prelude-packages-installed-p)
(message "%s" "Emacs Prelude is now refreshing its package database...")
(package-refresh-contents)
(message "%s" " package refresh is done")
(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p))))
(provide 'prelude-packages)

;; fuzzy matching. Seems a bit wonky with file find (C-x C-f)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; inferior ruby mode.
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; activate rvm
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

;; Interact with bundler
(require 'bundler)
(global-set-key (kbd "C-c r b i") 'bundle-install)
(global-set-key (kbd "C-c r b u") 'bundle-update)
(global-set-key (kbd "C-c r b c") 'bundle-console)

;; handle mixed html / programming language files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))

;; magit status
(global-set-key (kbd "M-`") 'magit-status)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)

;; turn off auto-compile for scss mode as it doesn't seem to work with
;; rails
(setq scss-compile-at-save nil)
