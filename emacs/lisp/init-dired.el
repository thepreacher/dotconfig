;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired-single
  :defer)

(use-package diredfl
  :hook ((dired-mode . diredfl-global-mode))
  :config
  (require 'dired-x))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-open
  :defer
  :init
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;;preview files in dired
(use-package peep-dired
  :defer ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
          ("P" . peep-dired)))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
          ("/" . dired-narrow)))

;; Tree style directory views in dired
(use-package dired-subtree
  :config
  :bind (:map dired-mode-map
          ("i" . dired-subtree-insert)
          (";" . dired-subtree-remove)))



(provide 'init-dired)
;;; init-dired.el ends here
