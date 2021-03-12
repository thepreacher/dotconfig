;;; init-web.el --- Support for the Web templates -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'init-html)

;; Web-mode - An autonomous emacs major-mode for editing web templates.

(use-package web-mode
  :delight "☸ "
  :hook ((css-mode web-mode) . rainbow-mode)
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.l?eex\\'" . web-mode)
         ("\\.php$" . my/php-setup))
  :init
  (setq web-mode-enable-block-face                    nil
        web-mode-enable-comment-annotation            t
        web-mode-enable-css-colorization              t
        web-mode-enable-current-column-highlight      t
        web-mode-enable-current-element-highlight     t
        web-mode-enable-inlays                        nil
        web-mode-enable-optional-tags                 t
        web-mode-enable-part-face                     t
        web-mode-enable-sexp-functions                nil
        web-mode-enable-sql-detection                 nil
        web-mode-enable-string-interpolation          t
        web-mode-enable-comment-interpolation         t
        web-mode-enable-heredoc-fontification         nil
        web-mode-enable-html-entities-fontification   nil
        web-mode-enable-element-content-fontification nil
        web-mode-enable-whitespace-fontification      nil
        web-mode-enable-auto-expanding                t
        web-mode-enable-control-block-indentation     t
        web-mode-enable-auto-indentation              t
        web-mode-enable-auto-closing                  nil
        web-mode-enable-auto-opening                  nil
        ;; make web-mode play nice with smartparens
        web-mode-enable-auto-pairing                  nil
        web-mode-enable-auto-quoting                  t)
  :custom
  (web-mode-code-indent-offset                   2)
  (web-mode-markup-indent-offset                 2)
  (web-mode-css-indent-offset                    2)
  (web-mode-markup-indent-offset                 2)
  (web-mode-code-indent-offset                   2)
  (web-mode-comment-style                        2)
  :config

  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))







(provide 'init-web-mode)
;;; init-web-mode.el ends here
