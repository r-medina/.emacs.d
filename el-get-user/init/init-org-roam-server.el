(require 'org-roam)
(let ((m org-roam-mode-map))
  (define-key m (kbd "C-c o S") 'org-roam-server-mode))
