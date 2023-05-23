;;; package --- summary This is my emacs noob config for a scala developer 
(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;;; standard configurations for things emacs does that annoy me:
;; don't create '#' prefixed lock files
(setq create-lockfiles nil)
;; save backups to a separate directory
(setq backup-directory-alist `(("." . "~/.emacs.d/.emacs.bak")))
;; turn off annoying sound at end of buffer on scroll
(setq ring-bell-function 'ignore)


;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; GUI specific Options
;; https://github.com/osener/emacs-afternoon-theme/tree/master
(use-package afternoon-theme
  :if window-system
  :init
  ;; Set default theme
  (load-theme 'afternoon t))


;; Vterm
(use-package vterm
 :commands vterm)
(use-package helm-switch-shell
 :after helm
 :bind ("s-v". helm-switch-shell)
 ("C-c v". helm-switch-shell)
 :config (setq helm-switch-shell-new-shell-type 'vterm))



;; Navigation
(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char2)
  ("s-g" . avy-goto-line)
  ("C-\"" . avy-goto-word-1))


(use-package display-line-numbers
  :demand t
  :config
  (defun display-line-numbers--turn-on ()
    "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
    (if (and
         (not (member major-mode '(treemacs)))
         (not (minibufferp)))
        (display-line-numbers-mode)))
  (global-display-line-numbers-mode))



;; load the personal settings (this includes `custom-file'
(defvar emacs-dir (file-name-directory load-file-name))
(defvar config-dir (expand-file-name "config" emacs-dir))
(when (file-exists-p config-dir)
  (message "Loading personal configuration files in %s..." config-dir)
  (mapc 'load (directory-files config-dir 't "^[^#\.].*\\.el$"))
  (message "Loading personal configuration files in %s...done" config-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dimmer resize-window transpose-frame which-key helpful helm-ag helm-flyspell flyspell-lazy burly bufler multiple-cursors git-link diff-hl git-timemachine code-review gh-notify forge treemacs-all-the-icons use-package-ensure-system-package eshell-vterm vterm-toggle multi-vterm ztree helm-switch-shell vterm afternoon-theme helm-lsp helm-company helm-swoop helm-projectile helm yasnippet vertico use-package treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired sbt-mode lsp-ui lsp-metals flycheck company))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;

(defun burly-bookmark-windows-with-projectile-name (name)
  "Bookmark the current frame's window configuration as NAME."
  (interactive
   (list (completing-read "Save Burly wbookmark: " (burly-bookmark-names)
                          nil nil  (concat burly-bookmark-prefix (projectile-project-name)) )))
  (let ((record (list (cons 'url (burly-windows-url))
                      (cons 'handler #'burly-bookmark-handler))))
    (bookmark-store name record nil)))

(defun bufler-custom-groups ()
  "Customized bufler groups"
  (bufler-defgroups
    (group
     ;; Subgroup collecting all named workspaces.
     (auto-workspace))
    (group
     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
     (group-or "*Help/Info*"
	       (mode-match "*Help*" (rx bos "help-"))
	       (mode-match "*Info*" (rx bos "info-"))))
    (group
     ;; Subgroup collecting all special buffers (i.e. ones that are not
     ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
     ;; through to other groups, so they end up grouped with their project buffers).
     (group-and "*Special*"
		(lambda (buffer)
		  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
				       buffer)
			      (funcall (mode-match "Dired" (rx bos "dired"))
				       buffer)
			      (funcall (mode-match "Vterm" (rx bos "vterm"))
				       buffer)
			      (funcall (auto-file) buffer))
		    "*Special*")))
     (group
      ;; Subgroup collecting these "special special" buffers
      ;; separately for convenience.
      (name-match "**Special**"
		  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     (group
      ;; Subgroup collecting all other Magit buffers, grouped by directory.
      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
      (auto-directory))
     ;; Subgroup for Helm buffers.
     (mode-match "*Helm*" (rx bos "helm-"))
     ;; Remaining special buffers are grouped automatically by mode.
     (auto-mode))
    ;; All buffers under "~/.emacs.d" (or wherever it is).
    (dir user-emacs-directory)
    (group
     ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
     ;; `org-directory' is not yet defined).
     (dir (if (bound-and-true-p org-directory)
	      org-directory
	    "~/org"))
     (group
      ;; Subgroup collecting indirect Org buffers, grouping them by file.
      ;; This is very useful when used with `org-tree-to-indirect-buffer'.
      (auto-indirect)
      (auto-file))
     ;; Group remaining buffers by whether they're file backed, then by mode.
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group-and "Projectile"
	       ;; Subgroup collecting buffers in a projectile project.
	       (auto-projectile))
    (group
     ;; Subgroup collecting buffers in a version-control project,
     ;; grouping them by directory.
     (auto-project))
    ;; Group remaining buffers by directory, then major mode.
    (auto-directory)
    (auto-mode)))

(use-package bufler
  :commands (bufler bufler-switch-buffer)
  :after (projectile ace-window)
  :init (bufler-mode)
  :config
   (setf bufler-groups (bufler-custom-groups))
   :bind
   ("C-x b" . bufler-switch-buffer)
   ("C-x C-b" . bufler))

;; (use-package helm-bufler
;;   :after (helm bufler)
;;   :config (helm :sources '(helm-bufler-source)))

(use-package burly
  :after projectile
  :ensure t
  :bind
  ("C-c w s" . burly-open-bookmark)
  ("C-c w c" . burly-bookmark-windows)
  ("C-c w l" . list-bookmarks)
  :config
  (advice-add 'burly-bookmark-windows :override #'burly-bookmark-windows-with-projectile-name))

(use-package helm
  :init (helm-mode 1)
  :bind
  ("C-x C-f" . helm-find-files)
  ; ("C-x b" . helm-mini)
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring))

(use-package helm-ag)

;; (use-package helm-projectile
;;   :after (helm projectile)
;;   :config (helm-projectile-on))
(use-package helm-projectile
  :demand t
  :after (helm projectile)
  :config
  (helm-projectile-on)
 (setq helm-source-projectile-projects-actions
	(add-to-list 'helm-source-projectile-projects-actions
		     '("Switch to Vterm `s-v`" .
		       (lambda (project)
			 (let
			     ((default-directory project))
			   (projectile-run-vterm))))
	     t)
	)
 )

(use-package helm-swoop
  :commands (helm-swoop)
  :after helm
  :bind
  ("C-c C-s" . helm-multi-swoop-all)
  ("C-c s" . helm-swoop))

;; Helm Company ;;
(use-package helm-company
  :commands (helm-company)
  :after (company helm)
  :init
  (setq helm-company-candidate-number-limit 3000)
  (bind-key "TAB" #'helm-company
            company-active-map))
(use-package helpful
  :demand t)

(use-package which-key
  :demand
  :init
  (which-key-mode 1))
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; (setq gc-cons-threshold 100000000) ;; 100mb
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; (setq lsp-idle-delay 0.500)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))


;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t
  :init
  ;; hack to get metals to load
  ;; https://github.com/Alexander-Miller/treemacs/issues/982#issuecomment-1291484963
  (require 'treemacs-extensions)
  :after lsp-mode
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  )

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui
  :after lsp-mode)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
;; (use-package company
;;   :hook (scala-mode . company-mode)
;;   :config
;;   (setq lsp-completion-provider :capf))

(use-package helm-lsp
  :after lsp-mode
  :hook (scala-mode . helm-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package openapi-yaml-mode
  :load-path "external-packages/openapi-yaml-mode"
  :defer t)

;;;

(use-package projectile
  :init (projectile-mode +1)
  :config (setq projectile-project-search-path '(("~/Projects/" . 2) ))
  :bind-keymap ("C-c p" . projectile-command-map)
  )

(use-package flycheck-projectile
  :commands flycheck-projectile-list-errors)

;; Treemacs mods
(use-package treemacs
  :defer t
  :bind
  (:map global-map
    ("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after (all-the-icons treemacs))

;;; treemacs.el ends here
;;;


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package forge
  :demand t
  :ensure t
  :after magit)

;;; prefix commit messages with jira ticket
(use-package git-commit-jira-prefix
  :load-path "libs/git-commit-jira-prefix"
  :after git-commit
  :commands git-commit-jira-prefix-init
  :init (git-commit-jira-prefix-init))

(use-package gh-notify
  :after forge
  :commands (gh-notify))

(use-package code-review
  :after (magit forge)
  :commands (code-review-start code-review-forge-pr-at-point)
  :bind ("C-c g r" . code-review-forge-pr-at-point))

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package diff-hl
  :after magit
  :init
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind ("C-c g l" . git-link))

(winner-mode 1)

;; Window Management  ;;
(use-package transpose-frame)

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind ("M-o" . ace-window)
  :config
  (setq aw-dispatch-always t)
  (advice-add 'aw--switch-buffer  :override #'bufler-switch-buffer))

(use-package resize-window
  :commands (resize-window)
  :bind ("s-w" . resize-window))

(use-package dimmer
  :demand t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
			     
  (dimmer-mode t))
