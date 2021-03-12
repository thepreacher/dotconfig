;;; init-auto-package-update.el --- Automatic Package Updates -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auto-package-update
  :hook (after-init . auto-package-update-maybe)
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  ;; (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


(provide 'init-auto-package-update)
