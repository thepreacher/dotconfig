;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))



(provide 'init-flycheck)
;;; init-flycheck.el ends here
