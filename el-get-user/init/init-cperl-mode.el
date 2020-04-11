(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 8)
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

(add-hook 'cperl-mode-hook
          (lambda()
            (require 'perl-completion)
            (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t)
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion)))))
