;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :after grep
  :bind (:map grep-mode-map
         ("C-c C-q" . wgrep-change-to-wgrep-mode)
         ("w" . wgrep-change-to-wgrep-mode)))


(use-package ag
  :ensure-system-package ag
  :config
  (use-package wgrep-ag)
  :init
  (setq-default ag-highlight-search t)
  :bind ("M-?" . ag-project))

(use-package rg
  :ensure-system-package rg
  :bind ("M-?" . rg-project))



(provide 'init-grep)
;;; init-grep.el ends here
