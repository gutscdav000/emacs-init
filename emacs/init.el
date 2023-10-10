;; this file extends the init.el file defined here:
;; https://github.com/gutscdav000/base-emacs-home-manager-module/blob/main/home-manager-modules/emacs/init.el

;; Python LSP test
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
