(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
(helm-mode 1)

