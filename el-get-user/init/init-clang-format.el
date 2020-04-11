(require 'clang-format)

(defun clang-format-before-save ()
  "Add this to .emacs to fun clang-format on the current buffer when saving:
 (add-hook 'before-save-hook 'clang-format-before-save)."

  (interactive)
  (when (eq major-mode 'c-mode) (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-format-before-save)
