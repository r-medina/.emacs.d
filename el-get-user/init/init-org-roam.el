(setq org-roam-directory "~/notes")

(defvar org-roam-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c o r") 'org-roam)
    (define-key m (kbd "C-c o f") 'org-roam-find-file)
    (define-key m (kbd "C-c o g") 'org-roam-show-graph)
    m)
  "keymap for org-roam")

