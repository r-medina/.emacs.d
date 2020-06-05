;; This file tries to do as little as possible before calling into a
;; literate org-mode file that actually configures my emacs.
;;
;; First thing I do here is configure use-package such that I can
;; update org-mode before loading babel.
;;
;; A lot of this comes from here: https://github.com/hrs/dotfiles/blob/master/emacs/dot-emacs.d/init.e

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package org
  :commands org-babel-load-file)

;; load real config
(require 'org)
(org-babel-load-file "~/.emacs.d/README.org")

;;; V.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("0a8b8aad17f3bd4af1839a49cd97048c90db98a088b1f6f6d0d2187a3c35321d" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "319bf1bab5d05e3a4c4a165efe69d27b3d975759034074f15fe61e92c7304884" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(hscroll-margin 8)
 '(hscroll-step 1)
 '(org-roam-server-mode t)
 '(package-selected-packages (quote (use-package)))
 '(tramp-syntax (quote simplified) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
