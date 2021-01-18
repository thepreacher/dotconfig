;;; init-shellscript.el --- Shellscript support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; Shell Script
(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :config
  (use-package shfmt)


  (provide 'init-shellscript))
  ;;; init-shellscript.el ends here
