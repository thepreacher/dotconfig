;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Projectile - Project Intereaction Library
(use-package projectile
  :init
  (add-hook 'after-init-hook 'projectile-mode)
  ;; https://docs.projectile.mx/projectile/configuration.html#alien-indexing
  ;; This code messes with consult when project is non-VCS
  ;; (when (executable-find "rg")
  ;;   (setq-default projectile-generic-command "rg --files --hidden"))
  ;; ;; NOTE: Set this to the folder where you keep your Git repos!
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
  :config
  ;; (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (nqa/leader-key-def
    "p"   '(:ignore t :which-key "projectile")
    "pf"  'projectile-find-file
    "ps"  'projectile-switch-project
    "pb"  'projectile-switch-to-buffer
    "pe"  'projectile-recentf
    "pj"  'projectile-find-tag
    "pl"  'projectile-find-file-in-directory
    "pg"  'projectile-grep
    "pc"  'projectile-compile-project
    "pD"  'projectile-dired))



(provide 'init-projectile)
;;; init-projectile.el ends here
