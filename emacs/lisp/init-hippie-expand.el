;;; init-hippie-expand.el --- Settings for hippie-expand -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; To bind hippie-expand to M-<spc> put the following in your .emacs
(global-set-key "\M- " 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(provide 'init-hippie-expand)
;;; init-hippie-expand.el ends here
