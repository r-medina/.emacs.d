(require 'heaven-and-hell)
(setq heaven-and-hell-theme-type 'dark)

;; Set preferred light and dark themes
;; default light is emacs default theme, default dark is wombat
;; Themes can be the list: (dark . (tsdh-dark tango-dark))
(setq heaven-and-hell-themes
      '((light . tango)
        (dark . atom-dark)))

;; Optionall, load themes without asking for confirmation.
(setq heaven-and-hell-load-theme-no-confirm t)

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)
