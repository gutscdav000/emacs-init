
(defun dgibs/project-in-new-tab ()
  "Open a project in a new tab and rename the tab"
  (interactive)
  (tab-new)
  (projectile-switch-project)
  (tab-rename (projectile-project-name)))

(defun dgibs/project-rename-tab-with-buffer ()
  "Rename the current tab with the name of the project of the current buffer"
  (interactive)
  (tab-rename (projectile-project-name)))

(use-package projectile
  :bind
  (("C-x t P" . dgibs/project-in-new-tab)
   ("C-x t R" . dgibs/project-rename-tab-with-buffer)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  )
