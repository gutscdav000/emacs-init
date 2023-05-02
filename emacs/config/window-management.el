(require 'use-package)

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
