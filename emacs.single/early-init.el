;;; early-init.el --- Emacs 27+ pre-initialisation config
;;  Author: napleon Ahiable
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

;;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))


(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))


;; Ref: https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(add-to-list 'initial-frame-alist '(width  . 96))
(add-to-list 'initial-frame-alist '(height . 58))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))

;;; Font Configuration
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "SF Pro Display" :height efs/default-variable-font-size :weight 'regular)



;; So we can detect this having been loaded
(provide 'early-init)
;; Local Variables:
;;; early-init.el ends here
