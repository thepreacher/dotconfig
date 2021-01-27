;;; init-snippets.el --- Snippets and code expansion config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emmet-mode
  :delight
  :hook (css-mode sgml-mode html-mode web-mode))

(use-package yasnippet
  :commands (yas-reload-all yas-minor-mode)
  :delight yas-minor-mode " υ"
  :init
  (setq-default yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :hook ((yas-minor-mode . my/disable-yas-if-no-snippets))
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1)))
  :config
  (yas-global-mode 1))



(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; Loren ipsum
(use-package lorem-ipsum
  :defer
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))




(provide 'init-snippets)
;;; init-snippets.el ends here
