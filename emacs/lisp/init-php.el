;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://github.com/felixfbecker/php-language-server
;; For people who wonder, I don’t use php-mode because it can’t handle files that
;; contain PHP and HTML. Also, why use another package when web-mode already provides everything I need?
;; The function below provides my own PHP configuration with flycheck.

(defun my/php-setup ()
  (web-mode)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset))

;; Don’t forget to add the following line in the web-mode package configuration:

;; (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))

;; I like to use ac-php to enable GNU Emacs auto-completion for PHP.
;; **NOTE:** ac-php supports company mode and auto-complete.

(use-package ac-php
  :after (company php-mode)
  :hook (php-mode . ac-php-mode)
  :custom (ac-sources '(ac-source-php))
  :config
  (ac-php-core-eldoc-setup)
  (auto-complete-mode t))


(provide 'init-php)
;;; init-php.el ends here
