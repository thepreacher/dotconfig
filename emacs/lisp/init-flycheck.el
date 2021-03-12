;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;; Optionally add the `consult-flycheck' command.
  (use-package consult-flycheck
    ;; :requires flycheck
    :after consult
    :bind (:map flycheck-command-map
            ("!" . consult-flycheck))))



(provide 'init-flycheck)
;;; init-flycheck.el ends here
