;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Markdown

(use-package markdown-mode
  :ensure-system-package (pandoc)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))




(provide 'init-markdown)
;;; init-markdown.el ends here
