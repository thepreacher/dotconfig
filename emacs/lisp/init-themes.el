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

  ; Set cursor color to white
  (set-cursor-color "#ffb86c")

  ;; Set here because the colours only make sense in the context of dracula theme
  (set-face-background 'show-paren-match "#ffb86c") ;; #262b36 previous
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff")

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))




(provide 'init-themes)
;;; init-themes.el ends here
