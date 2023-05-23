(require 'use-package)

(use-package projectile
  :init (projectile-mode +1)
  :config (setq projectile-project-search-path '(("~/Projects/" . 2) ))
  :bind-keymap ("C-c p" . projectile-command-map)
  )

(use-package flycheck-projectile
  :commands flycheck-projectile-list-errors)
