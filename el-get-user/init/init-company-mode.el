(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0)
;; start completing after a single character instead of 3
(setq company-minimum-prefix-length 1)
;; align fields in completions
(setq company-tooltip-align-annotations t)
