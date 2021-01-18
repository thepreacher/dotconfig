;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; CSS – LESS – SCSS

;; In order to have a fast and stable environment, I recommend using LSP as a client for LSP servers and vscode-css-languageserver-bin as server.
(use-package css-mode
  :mode "\\.css\\'"
  :init (setq css-indent-offset 2))

(use-package less-css-mode
  :mode "\\.less\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

;; CSV
(use-package csv-mode
  :mode ("\\.csv\\'"))



(provide 'init-css)
;;; init-css.el ends here
