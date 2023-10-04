(use-package jinx
        :ensure t
        :init
        (global-jinx-mode)
        :commands (global-jinx-mode jinx-mode jinx-correct jinx-languages)
        :bind
        ([remap ispell-word] . jinx-correct)
        ("C-M-$" . jinx-languages))
