;;; init-dashboard.el --- Support for the Dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Dashboard
(use-package dashboard
  :if (< (length command-line-args) 2)
  :preface
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 5) ?)
            (format "%d packages loaded in %s" (length package-activated-list) (emacs-init-time))))
  :init
  (add-hook 'after-init-hook 'dashboard-setup-startup-hook t)
  (progn
    (setq dashboard-items '((recents . 5)
                            (projects . 10)))
    (setq dashboard-banner-logo-title "Happy Coding!!!")
    (setq dashboard-startup-banner "~/Documents/napo_avatar_rounded.png")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-footer nil))
  :config
  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages)))






(provide 'init-dashboard)
;;; init-rust.el ends here
