;;; init-general.el --- General customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer nqa/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer nqa/ctrl-c-keys
    :prefix "C-c"))



(provide 'init-general)
