;;; init-modeline.el --- Modeline details -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Doom-modeline

;; Use `window-setup-hook' if the right segment is displayed incorrectly
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-bar-width 6)

  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t))




(provide 'init-modeline)
;;; init-modeline.el ends here
