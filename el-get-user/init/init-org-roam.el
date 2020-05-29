(require 'org-roam)
(setq org-roam-directory "~/notes")

(defvar org-roam-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c o r") 'org-roam)
    (define-key m (kbd "C-c o f") 'org-roam-find-file)
    ;; (define-key m (kbd "C-c o g") 'org-roam-graph)

    (define-key m (kbd "C-c o y") 'org-roam-dailies-yesterday)
    (define-key m (kbd "C-c o m") 'org-roam-dailies-tomorrow)
    (define-key m (kbd "C-c o d") 'org-roam-dailies-date)
    m)
  "keymap for org-roam")

(global-set-key (kbd "C-c o t") 'org-roam-dailies-today)

(global-set-key (kbd "C-c o c") 'org-roam-capture)
