(turn-on-pbcopy)

(defun pbcopy-selection-value ()
  "See `x-cut-buffer-or-selection-value'."
  (when pbcopy-program
    (let (clip-text primary-text)
      (when pbcopy-select-enable-clipboard
        (let ((tramp-mode nil)
              (default-directory "~"))
          (setq clip-text (shell-command-to-string "pbpaste")))
        (setq clip-text
              (cond ;; check clipboard selection
               ((or (not clip-text) (string= clip-text ""))
                (setq pbcopy-last-selected-text-primary nil))
               ((eq      clip-text pbcopy-last-selected-text-clipboard) nil)
               ((string= clip-text pbcopy-last-selected-text-clipboard)
                ;; Record the newer string,
                ;; so subsequent calls can use the `eq' test.
                (setq pbcopy-last-selected-text-clipboard clip-text)
                nil)
               (t (setq pbcopy-last-selected-text-clipboard clip-text)))))
      (let ((tramp-mode nil)
            (default-directory "~"))
        (setq primary-text (shell-command-to-string "pbpaste")))
      (setq primary-text
            (cond ;; check primary selection
             ((or (not primary-text) (string= primary-text ""))
              (setq pbcopy-last-selected-text-primary nil))
             ((eq      primary-text pbcopy-last-selected-text-primary) nil)
             ((string= primary-text pbcopy-last-selected-text-primary)
              ;; Record the newer string,
              ;; so subsequent calls can use the `eq' test.
              (setq pbcopy-last-selected-text-primary primary-text)
              nil)
             (t (setq pbcopy-last-selected-text-primary primary-text))))
      (or clip-text primary-text))))
