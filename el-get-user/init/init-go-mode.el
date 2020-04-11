(require 'go-mode)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook 'flycheck-mode)

(add-hook 'go-mode-hook 'lsp-deferred)
