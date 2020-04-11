(require 'yasnippet)
(yas-global-mode 1)

;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-keymap (kbd "TAB") nil)
;; (define-key yas-keymap (kbd "<backtab>") nil)
(define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
;; (define-key yas-keymap (kbd "<backtab>") 'yas-next-field)
