;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

;; (use-package tagedit)
;; (use-package sgml-mode
;;   :ensure nil
;;   :defer
;;   :config
;;   (tagedit-add-paredit-like-keybindings)
;;   (define-key tagedit-mode-map (kbd "M-?") nil)
;;   (define-key tagedit-mode-map (kbd "M-s") nil)
;;   (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

;; (add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; XML
(use-package xml-mode
  :ensure nil
  :mode ("\\.wsdl\\'" "\\.xsd\\'"))



(provide 'init-html)
;;; init-html.el ends here
