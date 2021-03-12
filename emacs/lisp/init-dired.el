;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil)

  (autoload 'dired-omit-mode "dired-x")

  (use-package dired-single
    :defer t)

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (use-package diredfl
    :hook ((dired-mode . diredfl-global-mode))
    :config
    (require 'dired-x))

  (use-package diff-hl
    :hook (dired-mode . diff-hl-dired-mode)
    :config
    (global-diff-hl-mode 1)
    (diff-hl-flydiff-mode 1))

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

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))


(defun nqa/dired-link (path)
  (lexical-let ((target path))
    (lambda () (interactive) (message "Path: %s" target) (dired target))))

(nqa/leader-key-def
  "d"   '(:ignore t :which-key "dired")
  "dd"  '(dired :which-key "Here")
  "dh"  `(,(nqa/dired-link "~") :which-key "Home")
  "dn"  `(,(nqa/dired-link "~/Notes") :which-key "Notes")
  "do"  `(,(nqa/dired-link "~/Downloads") :which-key "Downloads")
  "dp"  `(,(nqa/dired-link "~/Pictures") :which-key "Pictures")
  "dv"  `(,(nqa/dired-link "~/Videos") :which-key "Videos")
  "d."  `(,(nqa/dired-link "~/.dotfiles") :which-key "dotfiles")
  "de"  `(,(nqa/dired-link "~/.config/emacs") :which-key ".emacs.d"))



(provide 'init-dired)
;;; init-dired.el ends here
