;;; init-constants.el --- Useful constants to save typing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Constants
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")



(provide 'init-constants)
;;; init-constants.el ends here
