;;; init-snippets.el --- Snippets and code expansion config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emmet-mode
  :delight
  :hook (css-mode sgml-mode html-mode web-mode))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t))

(use-package yasnippet-snippets
  :after yasnippet)


;; Loren ipsum
(use-package lorem-ipsum
  :defer
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))




(provide 'init-snippets)
;;; init-snippets.el ends here
