;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Projectile - Project Intereaction Library
(use-package projectile
  ;; :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
      (setq projectile-project-search-path '("~/projects/learn/elixir/"
                                              "~/projects/learn/python/"
                                              "~/projects/learn/elm/"
                                              "~/projects/learn/rust/"
                                              "~/projects/learn/ai/"
                                              "~/projects/learn/c++/"
                                              "~/projects/learn/js/"
                                              "~/projects/probono/"
                                              "~/projects/paid/")))
  :bind-keymap (("C-x p" . projectile-command-map))
  :config
  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  ;; https://docs.projectile.mx/projectile/configuration.html#projectile-commander
  (setq projectile-switch-project-action 'projectile-commander)

  (def-projectile-commander-method ?\C-m
    "Find file in project."
    (call-interactively #'projectile-find-file))
  :config
  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default))
  ;; (setq projectile-known-projects-file (expand-file-name "var/projectile-bookmarks.eld" user-emacs-directory)))



(provide 'init-projectile)
;;; init-projectile.el ends here
