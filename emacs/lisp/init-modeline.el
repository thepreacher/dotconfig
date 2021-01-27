;;; init-modeline.el --- Modeline details -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Doom-modeline

;; Use `window-setup-hook' if the right segment is displayed incorrectly
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-lsp t)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t))
  



(provide 'init-modeline)
;;; init-modeline.el ends here
