(setq org-ellipsis "↩")

(add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c n i") 'org-roam-insert)))
