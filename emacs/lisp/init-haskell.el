;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Haskell
(use-package haskell-mode
  :mode ("\\.l?hs\\'")
  :hook (haskell-mode . turn-on-haskell-indentation)
  :config
  (use-package lsp-haskell))



(provide 'init-haskell)
;;; init-haskell.el ends here
