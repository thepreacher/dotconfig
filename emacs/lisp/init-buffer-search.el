;;; init-isearch.el --- Buffer search utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Efficient solution for single-buffer text search like Swiper
(use-package ctrlf
  :init
  (add-hook 'after-init-hook 'ctrlf-mode))


;; Avy move aroung buffer by search
(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(provide 'init-buffer-search)
;;; init-buffer-search.el ends here
