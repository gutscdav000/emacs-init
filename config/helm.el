(require 'use-package)

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
