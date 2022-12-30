(require 'use-package)

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
