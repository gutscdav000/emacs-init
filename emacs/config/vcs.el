;;;

(require 'use-package)

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
