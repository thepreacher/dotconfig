;;; init-use-package.el --- External package installation utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Saves from having to add :ensure t for every use-package declaration
(setq use-package-always-ensure t)

;; To aid benchmarking
;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)
(setq use-package-compute-statistics t)


(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update))

;; disable because of elpa bug in emacs 27 and 28
(setq package-check-signature nil)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.config/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Enable asynchronous compilation of (M)elpa packages
;; (use-package async
;;   :config
;;   (async-bytecomp-package-mode 1))

(use-package delight)
(use-package use-package-ensure-system-package)


;; Key chords intergration with use-package
(use-package use-package-chords
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)  ; 0.05 or 0.1
  (setq key-chord-one-key-delay 0.2))  ; 0.2 or 0.3 to avoid first autorepeat

;; (use-package esup :defer)



(provide 'init-packages)
;;; init-use-package.el ends here
