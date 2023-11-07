;; this gave me other issues and wasn't worth the squeeze
;; (use-package jinx
;;         :ensure t
;;         :init
;;         (global-jinx-mode)
;;         :commands (global-jinx-mode jinx-mode jinx-correct jinx-languages)
;;         :bind
;;         ([remap ispell-word] . jinx-correct)
;;         ("C-M-$" . jinx-languages))

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
