;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------

(use-package htmlize :defer)

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :defer t
  :hook ((org-mode
          emacs-lisp-mode
          web-mode
          typescript-mode
          js2-mode
          html-mode
          sass-mode
          css-mode) . rainbow-mode))

;; Try packages without installing
(use-package try :defer)


(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))


(provide 'init-misc)
;;; init-misc.el ends here
