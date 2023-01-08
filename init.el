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
