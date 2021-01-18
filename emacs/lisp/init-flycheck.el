;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-pylintrc "~/.pylintrc"
        flycheck-python-pylint-executable "/usr/bin/pylint"
        flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))




(provide 'init-flycheck)
;;; init-flycheck.el ends here
