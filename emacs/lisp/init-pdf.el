;;; init-pdf.el --- Support for the PDF docs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package pdf-tools
  :pin manual ;;manually update
  :init
  (add-hook 'after-init-hook 'pdf-tools-install)
  ;; this only has to be executed for the installation and can be removed/commented afterwards
  ;; I recommend commenting it out so that it can be found easily when reinstalling
  (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig")
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1)))
  :magic ("%PDF" . pdf-view-mode)
  ;; (pdf-tools-install :no-query))
  :config
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0))))


(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
          ("C-s" . isearch-forward)
          ("d" . pdf-annot-delete)
          ("h" . pdf-annot-add-highlight-markup-annotation)
          ("t" . pdf-annot-add-text-annotation))
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1
        pdf-view-use-unicode-ligther nil))


(provide 'init-pdf)
;;; init-pdf.el ends here
