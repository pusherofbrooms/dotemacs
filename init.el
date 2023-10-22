;;; package --- Summary
;;; Commentary:
;; This is an Emacs init for all of my programming tasks.
;; The base requirement is package.el, which is included in the Emacs 24
;; distribution.

;; I'm old, so I need bigger fonts!
(set-face-attribute 'default nil
                    :height 150
                    )

;; no gnu splash screen on startup
(setq inhibit-splash-screen t)

;; empty message in the scratch buffer on startup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; opens a buffer on a new file without confirmation (seen in C-x C-f)
(setq confirm-nonexistent-file-or-buffer nil)

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
(setq mode-require-final-newline nil)

;; Backup file buffer settings.
(setq
 backup-by-copying t                ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backups"))    ; don't litter my fs
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 0
 version-control t)                 ; use versioned backups

;; No toolbar, no menu.
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(menu-bar-mode 0)

;; mode-line customization
;; Show line number and column number in the mode line.
(line-number-mode 1)
(column-number-mode 1)

;; display time.
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time)

;; yes/no becomes y/n in any yes/no prompt.
(fset `yes-or-no-p 'y-or-n-p)

;; eshell customizations
(setq eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-history-size 1000
      eshell-buffer-maximum-lines 5000
      eshell-destroy-buffer-when-process-dies t)

;; ediff side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; prefer splitting windows horizontally (along the vertical axis)
(setq split-width-threshold 79)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(native-comp-async-report-warnings-errors nil)
 '(org-agenda-files '("~/notes.org"))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(
     auto-complete
     company
     company-irony
     company-quickhelp
     counsel
     csv-mode
     direnv
     exec-path-from-shell
     flycheck
     flycheck-irony
     irony-eldoc
     js2-mode
     magit
     markdown-mode
     multi-vterm
     org-ai
     platformio-mode
     popup
     projectile
     rust-mode
     toml-mode
     vterm
     yaml-mode
     yasnippet
     yasnippet-snippets
     web-mode
     )))

;; dired-x file handling
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    (setq dired-x-hands-off-my-keys nil)
	    ))

;; Pulled from emacs wiki and modified a bit.
;; "C-c d" inserts a yyyy-mm-dd date.
(defun insert-date (prefix)
  "Insert the current date.
  ISO format with C-u prefix.  mm-dd-yy with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d>")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d-%m-%Y"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

;; Similar to insert-date except that we add HH:MM:SS. Adjust the format
;; strings to taste. If you don't like seconds, remove the ":%S"
(defun insert-timestamp (prefix)
  "Insert the current timestamp.
  ISO format with C-u prefix.  common broken format with two C-u prefixes."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d %H:%M:%S>")
                 ((equal prefix '(4)) "%Y-%m-%d %H:%M:%S")
                 ((equal prefix '(16)) "%d-%m-%Y %H:%M:%S"))))
        (insert (format-time-string format))))
(global-set-key (kbd "C-c t") 'insert-timestamp)

;; org-mode visual customizations
(setq org-hide-emphasis-markers t)
(setq org-fontify-whole-heading-line t)
(setq org-src-fontify-natively t)
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-mode-hook #'toggle-word-wrap)
;; org-mode global keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (R . t)
   (python . t)))

;; Bind these files and file types to ruby
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; term settings
;; no more ansiterm now that we have vterm
;;(global-set-key [f1] 'ansi-term)
;; kill process on terminal exit. scraped from echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; C-y doesn't usually work in terminals. This should help.
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; if it isn't in elpa, I dump it in lisp/
;; like avr-asm-flymake. Apparently, this add to load-path must
;; be done after all packages are installed as the load-path is
;; overwritten.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;
;; Try to do any work that doesn't require outside packages before this point.
;;
(require 'package)
(package-initialize)
(package-install-selected-packages)

;; It's inelegant but I want to pick up the path from shell when on
;; os x. apparently, due to a current bug there isn't a way to set
;; the path for gui apps.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; vterm settings
(global-set-key [f1] 'multi-vterm)
(setq vterm-kill-buffer-on-exit t)
(setq vterm-max-scrollback 10000)

;; ivy completion
(require 'swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-wrap t)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f2> f") 'counsel-describe-function)
(global-set-key (kbd "<f2> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; handle mixed html / programming language files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; javascript
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; if you don't want to warn on missing semicolons,
;; uncomment the following
;; (setq js2-strict-missing-semi-warning t)

;; flycheck syntax checkers for python. flycheck will chose flake8 over pylint
;; if both are defined.
;; (setq flycheck-python-flake8-executable "~/.virtualenvs/emacsenv/bin/flake8")
;; (setq flycheck-python-pylint-executable "~/.virtualenvs/emacsenv/bin/pylint")
;; (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))

(require 'yasnippet)
;;(yas-global-mode 1)
(yas-reload-all)
(add-hook 'js2-mode-hook 'yas-minor-mode)
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

;; rust and lsp
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)
;; ensure rust-analyzer is installed. from the command line run:
;; rustup component add rust-analyzer
(add-hook 'rust-mode-hook 'eglot-ensure)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

;; c/c++ and platformio
(require 'company)
(require 'platformio-mode)
(add-to-list 'company-backends 'company-irony)

(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)
                           (platformio-conditionally-enable)))

(add-hook 'c-mode-hook (lambda ()
                         (irony-mode)
                         (irony-eldoc)
                         (platformio-conditionally-enable)))

(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)
            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)
            (irony-cdb-autosetup-compile-options)))

(add-hook 'flyckeck-mode-hook 'flycheck-irony-setup)

;; set company globally.
(setq company-tooltip-align-annotations t)
;; Don't downcase suggestions.
(setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; org-ai
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)

(provide 'init)
;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1)))))
