;; (require 'org)
;; (org-babel-load-file "~/.emacs.d/README.org")

;;; this file is going to be deprecated for README.org, until then however I'm
;;; keeping this to help me document and structure the literate configuration.

;;;; OLD


;;; Ricky Medina's emacs config file

;; 2020

;; github.com/r-medina
;; ricky.medina91@gmail.com

;; adds my stuff to load path
(add-to-list 'load-path "~/.emacs.d/usr")

;; save backups elsewhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;;; other package stuff

;; from https://github.com/hrs/dotfiles/blob/master/emacs/dot-emacs.d/init.el
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; getting packages right

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package ace-jump-mode
  :bind
  ("C-." . ace-jump-mode))

;; (use-package atom-dark-theme ; in el-get
;;   :commands (atom-dark atom-dark-theme))

(use-package beacon
  :commands (beacon-mode beacon-blink)
  :config
  (beacon-mode 1)
  :bind
  ("C-c b" . beacon-blink))

;; (use-package chrome.el ; in el-get
;;   :bind
;;   (:map chrome-mode-map
;; 	("M-r" . chrome-mark-tab)))

(use-package company
  :commands global-company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode) ;; TODO: does it need the #?
  (setq company-idle-delay 1)
  ;; starts completing after a single character instead of 3
  (setq company-minimum-prefix-length 1)
  ;; aligns fields in completions
  (setq company-tooltip-align-annotations t)
  (setq company-global-modes '(not markdown-mode)))
(use-package company-lsp)

(use-package diff-hl
  :commands
  (global-diff-hl-mode)
  :hook
  (diff-hl-mode . diff-hl-margin-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  :init
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package dockerfile-mode)

;; (use-package emacs-powerline) ;; TODO: am i using this? ;; in el-get

(use-package expand-region) ;; TODO: am i using this?

(use-package flycheck)

(use-package flyspell
  ;; :hook (prog-mode . flyspell-prog-mode)
  )

(use-package forge)

(use-package git-link
  :bind
  ("C-c l" . git-link))

(use-package go-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook
  (go-mode . 'flycheck-mode)
  (go-mode . 'lsp-deferred))
;; (use-package go-mod) ;; el-get
;; (use-package go-imports) ;; el-get

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package hcl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hcl.ctmpl\\'" . hcl-mode)))

(use-package heaven-and-hell
  :hook
  (after-init . heaven-and-hell-init-hook)
  :config
  (setq heaven-and-hell-themes
	'((light . tango)
	  (dark . atom-dark)))
  (setq heaven-and-hell-load-theme-no-confirm t))

(use-package helm
  :config
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-b" . helm-multi-files)
   ("C-x C-f" . helm-find-files)
   :map helm-find-files-map
   ;; TODO: look into \t vs <tab> vs TAB
   ("<tab>" . 'helm-execute-persistent-action)))

(use-package helm-projectile)

(use-package json-mode) ;; init in el-get

(use-package lsp-mode)
(use-package lsp-ui)

(use-package magit)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package org
  :no-require t
  :ensure org-plus-contrib
  ;; :commands (org
  ;;            org-capture
  ;;            org-mode
  ;;            org-store-link
  ;;            update-org-hours
  ;;            my-term-agenda
  ;;            dired-notes
  ;;            jump-to-org-agenda)
  :mode ("\\.org\\'" . org-mode)

  :config

  ;; (setq org-ellipsis "↩")
  ;; (setq org-todo-keywords
  ;; 	'((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE")
  ;; 	  (sequence "MAYBE")))
  ;; (setq org-directory "~/notes")
  ;; (setq org-agenda-files (list "~/notes"))
  ;; (setq org-tag-alist '(("work" . ?w)
  ;; 			("health" . ?h)
  ;; 			("family" . ?f)
  ;; 			("personal" . ?p)
  ;; 			("engineering-success" . ?e)))
  ;; (require 'org-tempo)

  ;; (use-package org-bullets
  ;;   :hook (org-mode . (lambda () (org-bullets-mode 1))))

  ;; (use-package org-roam
  ;;   :no-require t
  ;;   :hook
  ;;   (emacs-startup-hook . org-roam-mode)
  ;;   :config
  ;;   (setq org-roam-directory "~/notes")
  ;;   (use-package org-roam-server  
  ;;     :bind
  ;;     ;; (:map org-mode-map
  ;;     ;; 	;; TODO: update chart
  ;;     ;; 	("C-c o s" . org-roam-serever-mode)) 
  ;;     )

  ;;   :bind
  ;;   (("C-c o t" . org-roam-dailies-today)
  ;;    ("C-c o c" . org-roam-capture)
  ;;    ("C-c o f" . org-roam-find-file)
  ;;    :map org-mode-map
  ;;    ("C-c o r" . org-roam)
  ;;    ("C-c o y" . org-roam-dailies-yesterday)
  ;;    ("C-c o m" . org-roam-dailies-tomorrow)
  ;;    ("C-c o d" . org-roam-dailies-date)
  ;;    ("C-c o i" . org-roam-insert)))
  
  :hook
  (org-mode . flyspell-mode))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'javascript-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package pbcopy
  :config
  (turn-on-pbcopy)) ;; more in el-get, but don't think i need it

(use-package plantuml-mode)

(use-package protobuf-mode)

(use-package smart-mode-line
  :commands sml/setup
  :init
  (add-hook 'after-init-hook #'sml/setup))

(use-package switch-window
  :bind ("C-x o" . switch-window))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (customize-set-variable 'tramp-syntax 'simplified))

(use-package yaml-mode
  :commands yaml-mode)

(use-package yascroll
  :config
  (global-yascroll-bar-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  ("C-c y" . yas-expand))

;; end - getting packages right

;; ;; giving el-get a shot
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes/")
;; (setq el-get-user-package-directory "~/.emacs.d/el-get-user/init/")

;; (setq my-packages
;;       '(atom-dark-theme
;; 	chrome.el
;; 	emacs-powerline
;; 	go-mod
;; 	go-imports))

;; (ignore-errors (el-get-self-update)) ;; maybe bring this back?
;; ;; (el-get-update-all)
;; (el-get-cleanup my-packages) ;; deletes no-longer-listed packages

;; (el-get 'sync my-packages)

;; ;; fixes dired issue on startup
;; (if (eq system-type 'darwin)
;;     (setq insert-directory-program "gls" dired-use-ls-dired t))

;; ;; utf-8 for correct behavior in terminal
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; ;; so selections work like in everywhere else
;; (delete-selection-mode 1)

;; ;; for undo with window operations
;; ;; `C-c left/right`
;; (setq winner-mode t)

;; ;; fucking bell
;; (setq ring-bell-function #'ignore)

;; ;; reloads file if saved elsewhere
;; ;; docs here: https://www.gnu.org/software/emacs/manual/html_node/emacs/Reverting.html
;; (global-auto-revert-mode t)

;; ;;; II.  Programming/Modes

;; (setq ruby-insert-encoding-magic-comment nil)

;; ;; cleaning shit up
;; (defun my-clean ()
;;   (interactive)
;;   (setq end (point-max))
;;   "does some cleaning up of files"
;;   (indent-region 0 end)
;;   (delete-trailing-whitespace 0 end))

;; ;; so M-<Backspace> doesn't yank
;; (defun delete-word (arg)
;;   "Delete characters forward until encountering the end of a
;; word. With argument, do this that many times."
;;   (interactive "p")
;;   (delete-region (point) (progn (forward-word arg) (point))))

;; (defun backward-delete-word (arg)
;;   "Delete characters backward until encountering the end of a word.
;; With argument, do this that many times."
;;   (interactive "p")
;;   (delete-word (- arg)))

;; ;; http://kb.mit.edu/confluence/display/istcontrib/Disabling+the+Emacs+menubar%2C+toolbar%2C+or+scrollbar
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; (column-number-mode t)
;; ;; so lines don't get broken onto next line if longer than buffer
;; (set-default 'truncate-lines t)

;; ;; show matching parenthesis
;; (show-paren-mode t)

;; ;; fixing scrolling behavior to be less jumpy
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil)

;; (setq inhibit-splash-screen t)
;; (setq inhibit-startup-message t)

;; ;; theme

;; ;; ;; (load-theme 'atom-dark t)

;; ;;; IV.  Key bindings

;; ;; ;; (global-unset-key (kbd "C-q"))
;; ;; ;; (global-unset-key (kbd "C-f"))
;; ;; ;; (global-unset-key (kbd "C-b"))
;; ;; ;; (local-unset-key (kbd "C-j"))
;; ;; ;; (local-unset-key (kbd "a"))
;; ;; ;; (local-unset-key (kbd "e"))

;; (global-set-key (kbd "<M-DEL>") 'backward-delete-word)

;; ;; buffer navigation
;; (setq windmove-wrap-around t)
;; (define-prefix-command 'nav-map)
;; (let ((m nav-map))
;;   (define-key m (kbd "o") 'windmove-up)
;;   (define-key m (kbd "l") 'windmove-down)
;;   (define-key m (kbd "j") 'windmove-left)
;;   (define-key m (kbd "k") 'windmove-right))

;; ;; quick minor modes
;; (define-prefix-command 'quick-modes-map)
;; (let ((m quick-modes-map))
;;   (define-key m (kbd "w") 'whitespace-mode)
;;   (define-key m (kbd "l") 'display-line-numbers-mode)
;;   (define-key m (kbd "e") 'electric-pair-mode)
;;   (define-key m (kbd "f") 'flymake-mode)
;;   (define-key m (kbd "k") 'nav-text-minor-mode)
;;   (define-key m (kbd "p") 'paredit-mode)
;;   (define-key m (kbd "o") 'outline-minor-mode)
;;   (define-key m (kbd "d") 'diff-hl-mode)
;;   (define-key m (kbd "h") 'global-hl-line-mode)
;;   (define-key m (kbd "a") 'auto-complete-mode)
;;   (define-key m (kbd "t") 'toggle-truncate-lines))

;; ;; Outline-minor-mode key map
;; (define-prefix-command 'outline-map)
;; (let ((m outline-map))
;;   (define-key m (kbd "h") 'hide-sublevels)
;;   (define-key m (kbd "b") 'hide-body)
;;   (define-key m (kbd "a") 'show-all)
;;   (define-key m (kbd "c") 'hide-entry)
;;   (define-key m (kbd "e") 'show-entry))

;; ;; navigtation
;; (defvar nav-text-minor-mode-map
;;   (let ((m (make-sparse-keymap)))
;;     (suppress-keymap m t)
;;     (define-key m (kbd "j") 'backward-char)
;;     (define-key m (kbd "k") 'forward-char)
;;     (define-key m (kbd "l") 'previous-line)
;;     (define-key m (kbd ";") 'next-line)
;;     (define-key m (kbd "J") 'backward-word)
;;     (define-key m (kbd "K") 'forward-word)
;;     (define-key m (kbd "L") 'backward-paragraph)
;;     (define-key m (kbd ":") 'forward-paragraph)

;;     (define-key m (kbd "t") 'beginning-of-buffer)
;;     (define-key m (kbd "y") 'end-of-buffer)
;;     (define-key m (kbd "a") 'beginning-of-line)
;;     (define-key m (kbd "e") 'end-of-line)
;;     (define-key m (kbd "g") 'goto-line)
;;     (define-key m (kbd "G") 'ace-jump-mode)
;;     (define-key m (kbd "S") 'isearch-backward)
;;     (define-key m (kbd "s") 'isearch-forward)
;;     m)
;;   "nav-text-minor-mode keymap.")

;; (define-minor-mode nav-text-minor-mode
;;   "A minor mode so that my hands hurt less."
;;   nil " nav-text" 'nav-text-minor-mode-map)

;; ;; defining a minor mode for all my keys!!
;; ;; stolen from: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; (defvar my-keys-minor-mode-map
;;   ;; try make-sparse-keymap instead
;;   (let ((m (make-keymap)))
;;     (define-key m (kbd "C-j")   'ace-jump-mode)                ; jump to word with PREFIX
;;     (define-key m (kbd "C-u")   'magit-status)                 ; magit!
;;     (define-key m (kbd "C-t")   'comment-or-uncomment-region)  ; comment region
;;     (define-key m (kbd "M-P")   'package-list-packages)        ; listing packages
;;     (define-key m (kbd "M-E")   'el-get-list-packages)         ; listing packages
;;     (define-key m (kbd "M-S")   'shell)                        ; open shell
;;     (define-key m (kbd "C-c r") 'reload-file)                  ; reload file
;;     (define-key m (kbd "C-c c") 'my-clean)                     ; cleaning function
;;     (define-key m (kbd "C-c b") 'beacon-blink)
;;     (define-key m (kbd "C-c p") 'helm-projectile)
;;     (define-key m (kbd "C-c T") 'heaven-and-hell-toggle-theme) ; toggle theme

;;     (define-key m (kbd "C-c . e")       ; open .emacs
;;       ;; does this need interactive?
;;       (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
;;     (define-key m (kbd "C-c . z")       ; open .zshrc
;;       (lambda () (interactive) (find-file "~/.zshrc")))
;;     (define-key m (kbd "C-c . o")       ; open .zsh
;;       (lambda () (interactive) (find-file "~/.oh-my-zsh")))
;;     (define-key m (kbd "C-c . b")       ; open .bashrc
;;       (lambda () (interactive) (find-file "~/.bashrc")))
;;     (define-key m (kbd "C-c . i")       ; open init folder
;;       (lambda () (interactive) (find-file "~/.emacs.d/el-get-user/init")))
;;     (define-key m (kbd "C-c . p")       ; open .profile
;;       (lambda () (interactive) (find-file "~/.profile")))

;;     ;; experimenting

;;     (define-key m (kbd "C-c . C")       ; open Joe zoom
;;       (lambda () (interactive)
;;         (save-window-excursion
;; 	  (async-shell-command "TODO"))))

;;     (define-key m (kbd "C-c . c")       ; open Joe zoom
;;       (lambda () (interactive)
;;         (save-window-excursion
;; 	  (async-shell-command
;; 	   (concat
;; 	    "'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' "
;; 	    (read-string "url: "))))))

;;     (define-key m (kbd "C-c . j")       ; open Joe zoom
;;       (lambda () (interactive)
;;         (browse-url (getenv "JOE_ZOOM"))
;; 	(browse-url "https://www.facebook.com/groups/565308257695776/post_tags/?post_tag_id=566705834222685")))

;;     ;; work
;;     (define-key m (kbd "C-c . n")       ; open github notifications
;;       (lambda () (interactive)
;;         (browse-url "https://github.com/notifications")))

;;     (define-key m (kbd "C-n") nav-map)         ; navigation prefix
;;     (define-key m (kbd "C-o") outline-map)     ; outline prefix
;;     ;;     (define-key m (kbd "C-p") spotify-map)     ; spotify prefix
;;     (define-key m (kbd "M-m") quick-modes-map) ; quick modes prefix
;;     m)
;;   "my-keys-minor-mode keymap.")

;; ;; personal minor mode for key map. GREAT hack
;; (define-minor-mode my-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " my-keys" 'my-keys-minor-mode-map)
;; (my-keys-minor-mode 1)

;; ;; toggle my minor mode
;; (global-unset-key (kbd "M-m"))
;; (global-set-key (kbd "M-m m") 'my-keys-minor-mode)

;; ;;; V.
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes (quote (tango)))
;;  '(custom-safe-themes
;;    (quote
;;     ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
;;  '(package-selected-packages (quote (org-plus-contrib use-package)))
;;  '(tramp-syntax (quote simplified) nil (tramp)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (yasnippet yascroll yaml-mode switch-window smart-mode-line protobuf-mode plantuml-mode pbcopy paredit org-roam-server org-roam use-package org-plus-contrib org-bullets org lsp-ui json-mode helm-projectile heaven-and-hell hcl-mode golden-ratio go-mode git-link forge flycheck expand-region emacsql-sqlite3 dockerfile-mode diff-hl company-lsp beacon auto-compile ace-jump-mode)))
 '(tramp-syntax (quote simplified) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
