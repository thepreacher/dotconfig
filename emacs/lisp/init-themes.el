;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Icons - Reqired by some themes
(use-package all-the-icons
  :defer
  :if (display-graphic-p)
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))


;; Doom
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))




(provide 'init-themes)
;;; init-themes.el ends here
