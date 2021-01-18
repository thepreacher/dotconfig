;;; init-use-package.el --- External package installation utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
;; To aid benchmarking
;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;; (let ((package-check-signature nil))
;;   (use-package gnu-elpa-keyring-update))

;; disable because of elpa bug in emacs 27 and 28
(setq package-check-signature nil)

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



(provide 'init-use-package)
;;; init-use-package.el ends here
