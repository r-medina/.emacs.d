(setq org-ellipsis "↩")

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c o i") 'org-roam-insert)))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE")
	(sequence "MAYBE")))

(setq org-directory "~/notes")
(setq org-agenda-files (list "~/notes"))

(add-hook 'org-mode-hook 'flyspell-mode)
(require 'org-tempo)

(setq org-tag-alist '(("work" . ?w) ("health" . ?h) ("family" . ?f)))
