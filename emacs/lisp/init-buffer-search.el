;;; init-isearch.el --- Buffer search utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Efficient solution for single-buffer text search like Swiper
(use-package ctrlf
  :init
  (add-hook 'after-init-hook 'ctrlf-mode))


;; Avy move aroung buffer by search
(use-package avy
  :commands (avy-goto-char-timer avy-goto-char avy-goto-word-0 avy-goto-line)
  :bind ("C-;" . avy-goto-char-timer)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(nqa/leader-key-def
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))


(provide 'init-buffer-search)
;;; init-buffer-search.el ends here
