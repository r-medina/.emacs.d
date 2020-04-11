;;; dim-others.el --- A minor mode for dimming unselected buffers.

;; 2015
;; github.com/r-medina

;;; Code:

(require 'cl)

(defgroup dim-others nil
  "dim-others customization group"
  :group 'convenience)

(defun dim-others-dim-buffer (buffer)
  "dim a specified buffer"
  (set-buffer buffer)
  ;; (hl-line-mode -1)
  (font-lock-unfontify-buffer))

(defun dim-others-undim-buffer (buffer)
  "undim a specified buffer"
  (set-buffer buffer)
  (font-lock-fontify-buffer)
  ;;(hl-line-mode 1)
  )

(defun dim-others-visible-buffers ()
  "get all the visible buffers"
  (reduce 'cons (mapcar (lambda (frame) (buffer-list frame))
                        (visible-frame-list))))

(defun dim-others-dim-buffers ()
  "dim all inactive visible buffers"
  (interactive)
  (let ((b (current-buffer)))
    (mapcar 'dim-others-dim-buffer (cdr (dim-others-visible-buffers)))
    (dim-others-undim-buffer b)))

(defun dim-others-undim-buffers ()
  (interactive)
  (let ((b (current-buffer)))
    (mapcar 'dim-others-undim-buffer (cdr (dim-others-visible-buffers)))
    (set-buffer b)))

(define-minor-mode dim-others-mode
  "A minor mode for dimming unselected buffers."
  :lighter " dimming" ;; modeline indicator
  :global t
  :group 'dim-others)


(defun dim-others-hook ()
  (when dim-others-mode
    (dim-others-dim-buffers)))

(defun dim-others-add-hooks ()
  "initialize dim-others-mode"
  (mapcar (lambda (arg)
	    (add-hook arg (lambda ()
			    (when dim-others-mode (dim-others-dim-buffers)))))
	  '(buffer-list-update-hook buffer-quit-function))
  (add-hook 'dim-others-mode-hook
            (lambda () (if dim-others-mode
                           (dim-others-dim-buffers)
                         (dim-others-undim-buffers)))))

(provide 'dim-others)
