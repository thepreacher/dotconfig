;;; init-yaml.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Yaml
(use-package yaml-mode
  :delight "ψ "
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode))



(provide 'init-yaml)
;;; init-yaml.el ends here
