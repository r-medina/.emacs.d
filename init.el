;; (require 'org)
;; (org-babel-load-file "~/.emacs.d/README.org")

;;; this file is going to be deprecated for README.org, until then however I'm
;;; keeping this to help me document and structure the literate configuration.

;;;; OLD


;;; Ricky Medina's emacs config file

;; 2020

;; github.com/r-medina
;; ricky.medina91@gmail.com

;;; TODO:
;;
;; - fix stripe-buffer and rainbow delims recipes
;; - fix ipython notebook stuff
;; - edit find-file-hook such that it calls auto-complete-mode
;; - fix that my-clean leaves some trailing whitespace
;; - finish going through and downloading packages in el-get
;; - get swank running, learn snippets, expand region
;; - learn org-mode
;; - get better with yanking

;;; I.  Essentials

;; adds my stuff to load path
(add-to-list 'load-path "~/.emacs.d/usr")

;; save backups elsewhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; giving el-get a shot
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes/")
(setq el-get-user-package-directory "~/.emacs.d/el-get-user/init/")

(setq my-packages
      '(ace-jump-mode
        atom-dark-theme
        beacon
        bufler
        company-mode
        company-lsp
        ;; diff-hl
        dockerfile-mode
        emacs-powerline
        expand-region
        flycheck
        ;; flycheck-protobuf
        flymake
        flymake-cursor
        flymake-shell
        flyspell
        forge
        git-link

        go-mode
        go-mod
        go-imports

        ;; golden-ratio
        hcl-mode
	heaven-and-hell
        helm
        helm-projectile
        ;; hl-line+
        json-mode
        lsp-mode
        lsp-ui
        magit
        ;; magit-tramp
        markdown-mode
        ;; multi-web-mode
        org-mode
        org-bullets
        org-roam
        org-roam-server
        paredit
        pbcopy
        plantuml-mode
	protobuf-mode
        rainbow-delimiters
        simple-httpd
	;; smart-mode-line
        switch-window
        yaml-mode
	yascroll
        yasnippet))

(ignore-errors (el-get-self-update)) ;; maybe bring this back?
;; (el-get-update-all)
(el-get-cleanup my-packages) ;; deletes no-longer-listed packages

(el-get 'sync my-packages)

;; emacs package manager
(require 'package)

;; add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; exp
;; (setq package-list '(smart-mode-line))

                                        ; fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))

                                        ; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;;; end exp

;; fixes dired issue on startup
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))

(turn-on-pbcopy)

;; utf-8 for correct behavior in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; better buffer naming behavior
(require 'uniquify)

;; (require 'dim-others)
;; (dim-others-mode 1)
;; (dim-others-add-hooks)

;; so selections work like in everywhere else
(delete-selection-mode 1)

;; for undo with window operations
;; `C-c left/right`
(setq winner-mode t)

;; fucking bell
(setq ring-bell-function #'ignore)

;; faster tramp
(setq tramp-default-method "ssh")
(customize-set-variable 'tramp-syntax 'simplified)

;; (add-hook 'find-file-hook 'turn-on-visual-line-mode)
;; (add-hook 'find-file-hook 'diff-hl-mode)

(global-auto-revert-mode t)

;;; II.  Programming/Modes

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; c comments
(add-hook 'c-mode-hook (lambda () (setq comment-start "// "
                                        comment-end   "")))

;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired nil))

;; cleaning shit up
(defun my-clean ()
  (interactive)
  (setq end (point-max))
  "does some cleaning up of files"
;;   (untabify 0 end)
  (indent-region 0 end)
  (delete-trailing-whitespace 0 end))

;; how to relead file
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

;; so M-<Backspace> doesn't yank
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a
word. With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; no tabs
;; (setq-default indent-tabs-mode nil)

;; struggling with tabs
;; (setq default-tab-width 8)

;; java stuffs
;; not in el-get
;; (autoload 'malabar-mode "malabar-mode"
;;   "Better Java major mode" t)
;; (when (fboundp 'malabar-mode)
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;;     (add-hook 'malabar-mode-hook 'auto-complete-mode)))

;; org-roam
;; from https://org-roam.github.io/org-roam/manual/Getting-Started.html#Getting-Started
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-agenda-files (list "~/notes"))

;; scala
(add-hook 'scala-mode-hook 'auto-complete-mode)

;; matlab mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; python auto-complete
(add-hook 'jedi-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)

;; auto-completes should be done like init-paredit-mode.el

;; javascript
(add-hook 'javascript-mode-hook 'auto-complete-mode)

;; ocaml complete
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(defun ac-ocaml-candidates (prefix)
  "Candidates for OCaml auto-completion"
  (let ((candidates)
        (module-name
         (when (string-match "\\([A-Za-z_][A-Za-z0-9_']*\\)[.]" prefix)
           (match-string 1 prefix))))
    (if module-name
        (iter '(lambda (sym) (push (concat module-name "." sym) candidates))
              (ocaml-module-symbols (assoc module-name (ocaml-module-alist))))
      (iter
       '(lambda (mod)
          (iter '(lambda (sym) (push sym candidates))
                (ocaml-module-symbols mod)))
       (ocaml-visible-modules))
      (iter '(lambda (mod) (push (car mod) candidates)) (ocaml-module-alist)))
    candidates))

;; ;; auto-complete for ocaml
;; (ac-define-source ocaml
;;   '((available . (require 'caml-help nil t))
;;     (candidates . (ac-ocaml-candidates ac-prefix))
;;     (prefix . "\\(?:[^A-Za-z0-9_.']\\|\\`\\)\\(\\(?:[A-Za-z_][A-Za-z0-9_']*[.]\\)?[A-Za-z0-9_']*\\)")
;;     (symbol . "s")))

;; FIX THIS SOON
;; Completion words longer than 4 characters
;; (custom-set-variables
;;  '(ac-ispell-requires 4))
;; (eval-after-load "auto-complete"
;;   '(progn
;;   (ac-ispell-setup)))
;; (defun my/enable-ac-ispell ()
;;   (add-to-list 'ac-sources 'ac-source-ispell))


;;; III. Look

;; http://kb.mit.edu/confluence/display/istcontrib/Disabling+the+Emacs+menubar%2C+toolbar%2C+or+scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(beacon-mode 1)

;; theme

(setq my-dark-theme 'atom-dark)
(setq my-light-theme 'tsdh-light)

;; (load-theme 'atom-dark t)

;; column-number-mode
(column-number-mode t)

;; show matching parenthesis
(show-paren-mode t)

(set-default 'truncate-lines t)

;; prettier mode-line
;; (require 'smart-mode-line)
;; (if after-init-time (sml/setup)
;;   (add-hook 'after-init-hook 'sml/setup))

;; linum mode!
;; (global-linum-mode t)

;; for better linum mode formatting
(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         ;; my own formatting string
         (format (concat "%" (number-to-string width) "d\u2502 ")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))

;; fixing scrolling behavior to be less jumpy
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;; IV.  Key bindings

;; (global-unset-key (kbd "C-q"))
;; (global-unset-key (kbd "C-f"))
;; (global-unset-key (kbd "C-b"))
;; (local-unset-key (kbd "C-j"))
;; (local-unset-key (kbd "a"))
;; (local-unset-key (kbd "e"))

;; buffer views
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'helm-multi-files)

(global-set-key (kbd "<M-DEL>") 'backward-delete-word)
(global-set-key (kbd "C-c l") 'git-link)

;; buffer navigation
(setq windmove-wrap-around t)
(define-prefix-command 'nav-map)
(let ((m nav-map))
  (define-key m (kbd "o") 'windmove-up)
  (define-key m (kbd "l") 'windmove-down)
  (define-key m (kbd "j") 'windmove-left)
  (define-key m (kbd "k") 'windmove-right))

;; quick minor modes
(define-prefix-command 'quick-modes-map)
(let ((m quick-modes-map))
  (define-key m (kbd "w") 'whitespace-mode)
  ;; (define-key m (kbd "r") 'rainbow-delimiters-mode)
  (define-key m (kbd "l") 'display-line-numbers-mode)
  (define-key m (kbd "e") 'electric-pair-mode)
  (define-key m (kbd "f") 'flymake-mode)
  (define-key m (kbd "c") 'flycheck-mode)
  (define-key m (kbd "k") 'nav-text-minor-mode)
  (define-key m (kbd "p") 'paredit-mode)
  (define-key m (kbd "o") 'outline-minor-mode)
  ;; (define-key m (kbd "d") 'dim-others-mode)
  ;; (define-key m (kbd "d") 'diff-hl-mode)
  (define-key m (kbd "a") 'auto-complete-mode)
  (define-key m (kbd "v") 'visual-line-mode)
  (define-key m (kbd "t") 'toggle-truncate-lines))

;; Outline-minor-mode key map
(define-prefix-command 'outline-map)
(let ((m outline-map))
  (define-key m (kbd "h") 'hide-sublevels)
  (define-key m (kbd "b") 'hide-body)
  (define-key m (kbd "a") 'show-all)
  (define-key m (kbd "c") 'hide-entry)
  (define-key m (kbd "e") 'show-entry))

;; navigtation
(defvar nav-text-minor-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m t)
    (define-key m (kbd "j") 'backward-char)
    (define-key m (kbd "k") 'forward-char)
    (define-key m (kbd "l") 'previous-line)
    (define-key m (kbd ";") 'next-line)
    (define-key m (kbd "J") 'backward-word)
    (define-key m (kbd "K") 'forward-word)
    (define-key m (kbd "L") 'backward-paragraph)
    (define-key m (kbd ":") 'forward-paragraph)

    (define-key m (kbd "t") 'beginning-of-buffer)
    (define-key m (kbd "y") 'end-of-buffer)
    (define-key m (kbd "a") 'beginning-of-line)
    (define-key m (kbd "e") 'end-of-line)
    (define-key m (kbd "g") 'goto-line)
    (define-key m (kbd "G") 'ace-jump-mode)
    (define-key m (kbd "S") 'isearch-backward)
    (define-key m (kbd "s") 'isearch-forward)
    m)
  "nav-text-minor-mode keymap.")

(define-minor-mode nav-text-minor-mode
  "A minor mode so that my hands hurt less."
  nil " nav-text" 'nav-text-minor-mode-map)

;; defining a minor mode for all my keys!!
;; stolen from: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  ;; try make-sparse-keymap instead
  (let ((m (make-keymap)))
    (define-key m (kbd "C-j")   'ace-jump-mode)                ; jump to word with PREFIX
    (define-key m (kbd "C-u")   'magit-status)                 ; magit!
    (define-key m (kbd "C-t")   'comment-or-uncomment-region)  ; comment region
    (define-key m (kbd "M-P")   'package-list-packages)        ; listing packages
    (define-key m (kbd "M-E")   'el-get-list-packages)         ; listing packages
    (define-key m (kbd "M-S")   'shell)                        ; open shell
    (define-key m (kbd "C-c r") 'reload-file)                  ; reload file
    (define-key m (kbd "C-c c") 'my-clean)                     ; cleaning function
    (define-key m (kbd "C-c b") 'beacon-blink)
    (define-key m (kbd "C-c p") 'helm-projectile)
    (define-key m (kbd "C-c T") 'heaven-and-hell-toggle-theme) ; toggle theme

    (define-key m (kbd "C-c . e")       ; open .emacs
      ;; does this need interactive?
      (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    (define-key m (kbd "C-c . z")       ; open .zshrc
      (lambda () (interactive) (find-file "~/.zshrc")))
    (define-key m (kbd "C-c . o")       ; open .zsh
      (lambda () (interactive) (find-file "~/.oh-my-zsh")))
    (define-key m (kbd "C-c . b")       ; open .bashrc
      (lambda () (interactive) (find-file "~/.bashrc")))
    (define-key m (kbd "C-c . i")       ; open init folder
      (lambda () (interactive) (find-file "~/.emacs.d/el-get-user/init")))
    (define-key m (kbd "C-c . p")       ; open .profile
      (lambda () (interactive) (find-file "~/.profile")))
    (define-key m (kbd "C-c . j")       ; open Joe zoom
      (lambda () (interactive)
        (browse-url (getenv "JOE_ZOOM"))))

    (define-key m (kbd "C-n") nav-map)         ; navigation prefix
    (define-key m (kbd "C-o") outline-map)     ; outline prefix
    ;;     (define-key m (kbd "C-p") spotify-map)     ; spotify prefix
    (define-key m (kbd "M-m") quick-modes-map) ; quick modes prefix
    m)
  "my-keys-minor-mode keymap.")

;; personal minor mode for key map. GREAT hack
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;; toggle my minor mode
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m m") 'my-keys-minor-mode)

;;; V.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.1)
 '(ac-auto-start t)
 '(ac-delay 1e-05)
 '(ahs-idle-interval 0.001)
 '(blink-cursor-delay 0.2)
 '(blink-cursor-interval 0.3)
 '(c-basic-offset 8)
 '(clang-format-style
   "{BasedOnStyle: WebKit, IndentWidth: 8, AlignTrailingComments: true, PointerAlignment: Right, AlwaysBreakAfterDefinitionReturnType: true}")
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("319bf1bab5d05e3a4c4a165efe69d27b3d975759034074f15fe61e92c7304884" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default)))
 '(diff-hl-flydiff-delay 0.01)
 '(diff-hl-flydiff-mode t)
 '(diff-hl-margin-mode t)
 '(dired-use-ls-dired (quote unspecified))
 '(electric-pair-mode t)
 '(fill-column 80)
 '(flycheck-display-errors-delay 1e-05)
 '(flymake-cursor-auto-enable t)
 '(hscroll-margin 8)
 '(hscroll-step 1)
 '(js2-basic-offset 2)
 '(js2-highlight-level 3)
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (flycheck-protobuf protobuf-mode websocket web-beautify web w3 tt-mode stripe-buffer spotify smart-tabs-mode simple-httpd scss-mode request python-environment pylint projectile popup perl-completion packed oauth2 multi-term malabar-mode json-reformat javadoc-lookup ipython heroku haml-mode hackernews flymake-gjshint flymake-easy flymake-csslint flatland-theme epc emojify circe caml auto-dim-other-buffers anything alert airline-themes ace-jump-mode)))
 '(perl-tab-always-indent nil)
 '(recenter-positions (quote (top middle bottom)))
 '(safe-local-variable-values (quote ((require-final-newline . t))))
 '(switch-window-relative nil)
 '(tramp-default-method "ssh")
 '(tramp-syntax (quote simplified) nil (tramp))
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:background "CadetBlue" :underline t))))
 '(ahs-edit-mode-face ((t (:background "Coral3"))))
 '(ahs-face ((t (:background "white"))))
 '(ahs-plugin-bod-face ((t (:background "DodgerBlue"))))
 '(ahs-plugin-defalt-face ((t (:background "brightwhite" :underline t))))
 '(flyspell-duplicate-face ((t (:underline "chartreuse2" :weight bold))))
 '(flyspell-incorrect-face ((t (:underline "dark red" :weight bold)))))
