(require 'chrome)
(let ((m chrome-mode-map))
  (define-key m (kbd "M-r") 'chrome-mark-tab))
