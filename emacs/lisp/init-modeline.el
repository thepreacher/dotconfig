;;; init-modeline.el --- Modeline details -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Doom-modeline
(use-package doom-modeline
  :init
  (add-hook 'after-init-hook 'doom-modeline-init)
  (setq doom-modeline-height 15)
  (setq doom-modeline-lsp t))


(provide 'init-modeline)
;;; init-modeline.el ends here
