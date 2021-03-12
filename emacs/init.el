;;; init.el --- GNU Emacs Configuration -*- lexical-binding: t -*-
;;; Author: Napleon Ahiable
;;; Commentary:
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org#about
;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el

;; Targeted for Emacs 27+

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;; Path for my configs
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Don't clutter user-emacs-directory with package and custom settings info
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                             file-name-handler-alist file-name-handler-alist-original))))

;; Dashboard package provides save feature hence I've commented this out. Uncomment when not using Dashboard
(add-hook 'emacs-startup-hook
    #'(lambda ()
        (message "%d packages loaded in %s with %d garbage collections."
            (length package-activated-list)
            (format "%.2f seconds"
             (float-time
               (time-subtract after-init-time before-init-time)))
            gcs-done)))


(require 'package)
(require 'cl-lib)

;; Package sources
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;----------------------------------------------------------------------------
;; Load configs for Use-Package - package configuration utility
;;----------------------------------------------------------------------------
(require 'init-packages)
(require 'init-general)
;;----------------------------------------------------------------------------
;; Load configs for vanila emacs without any external packages
;;----------------------------------------------------------------------------
(require 'init-emacs)


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(use-package command-log-mode :defer) ;; Useful for screen casting
;;
(require 'init-constants)
;; (require 'init-frame-hooks)
;;
(when *is-a-mac*
  (require 'init-macos))
(require 'init-themes)
(require 'init-modeline)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-buffer-search)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-buffers)
;;
(require 'init-evil-mode)
;;
(require 'init-selectrum)
(require 'init-hippie-expand)
(require 'init-windows)
;; (require 'init-sessions)
;;
(require 'init-flycheck)
(require 'init-editing-utils)
(require 'init-snippets)
;;
;; (require 'init-vc)
;; (require 'init-darcs)
(require 'init-git)
;; (require 'init-github)
;;
(require 'init-projectile)
;;
(require 'init-compile)
(require 'init-lsp)
(require 'init-company)
(require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
;; (require 'init-html)
(require 'init-css)
(require 'init-python)
(require 'init-haskell)
(require 'init-elm)
(require 'init-elixir)
(require 'init-sql)
(require 'init-rust)
(require 'init-ccpp)
;; (require 'init-toml)
(require 'init-yaml)
(require 'init-docker)
;;
;; (require 'init-paredit)
(require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)
;; (require 'init-common-lisp)
(require 'init-web-mode)
;; (require 'init-pdf)
;;

(require 'init-treemacs)
(require 'init-misc)
;; (require 'init-dashboad)
(require 'init-auto-package-update)



;;----------------------------------------------------------------------------
;; Load configs work better if loaded last
;;----------------------------------------------------------------------------
;; Display line numbers (For some reason this doesn't have any effect if not set last *investigate)
(setq-default display-line-numbers-type t ;; for relative numbering set this to 'visual or 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

;; This should be at the bottom for it to work
(set-face-attribute 'line-number nil
            :font "JetBrainsMono Nerd Font")

(set-face-attribute 'line-number-current-line nil
            :weight 'bold
            :font "JetBrainsMono Nerd Font"
            :foreground "goldenrod")

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Allow access from emacsclient
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))


;; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/projects/"))

;;; Locales and UTF-8
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
