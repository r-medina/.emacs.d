(require 'diff-hl)
(global-diff-hl-mode)

;; (add-hook 'diff-hl-mode-hook 'diff-hl-flydiff-mode)
;; (add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
