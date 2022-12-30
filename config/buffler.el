(require 'use-package)

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
