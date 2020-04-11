(autoload 'pyret-mode "pyret" "Pyret language major mode." t)
(add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
(add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8))
