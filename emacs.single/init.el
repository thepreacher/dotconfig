;;; init.el --- GNU Emacs Configuration -*- lexical-binding: t -*-
;;; Author: Napleon Ahiable
;;; Commentary:
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org#about
;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el

;; Targeted for Emacs 27+

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;; Don't clutter user-emacs-directory with package and custom settings info
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))

;; Make startup faster by reducing the frequency of garbage
;; collection.

(defvar file-name-handler-alist-original file-name-handler-alist)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook ; hook run after loading init files
      #'(lambda ()
          (setq gc-cons-threshold normal-gc-cons-threshold
           gc-cons-percentage 0.1
           file-name-handler-alist file-name-handler-alist-original))))

;; Dashboard package provides save feature hence I've commented this out. Uncomment when not using Dashboard
(add-hook 'emacs-startup-hook
    #'(lambda ()
        (message "%d packages loaded in %s with %d garbage collections."
            (length package-activated-list)
            (format "%.2f seconds"
             (float-time
               (time-subtract after-init-time before-init-time)))
            gcs-done)))

(require 'package)
(require 'cl-lib)

;; Package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)



(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; (let ((package-check-signature nil))
;;   (use-package gnu-elpa-keyring-update))

;; disable because of elpa bug in emacs 27 and 28
(setq package-check-signature nil)


(use-package delight)
(use-package use-package-ensure-system-package)

;; To aid benchmarking
;; Show a message whenever a package takes longer than 0.1s to load
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(use-package async
  :config
  (async-bytecomp-package-mode 1))

;; Authentication
;; put my authinfo.gpg file in Syncthing in order to be able to easily use my configuration on other devices.
(use-package auth-source
  :no-require t
  :config
  (setq auth-sources '("~/.config/gnupg/shared/authinfo.gpg"
                       "~/.authinfo.gpg"
                       "~/.authinfo"
                       "~/.netrc")))

;;============================================================================;;


;;; Make environment variables available in Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-debug nil)
  ;; (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (progn
    (dolist (var '("SSH_AUTH_SOCK"
                   "LANG"
                   "WORKON_HOME"))
      (add-to-list 'exec-path-from-shell-variables var)))
  (exec-path-from-shell-initialize))


;; Constants
(defconst *sys/mac*
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


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; Better defaults
(setq-default
  visible-bell t                                   ; Set up the visible bell
  create-lockfiles nil                             ; Do not create lockfiles
  auto-save-default nil                            ; Do not save files automatically
  frame-resize-pixelwise t                         ; Set frame size pixelwise instead of characterwise
  save-interprogram-paste-before-kill t            ;
  mouse-yank-at-point t                            ;
  enable-recursive-minibuffers t                   ; Allow commands to be run on minibuffers
  kill-whole-line t                                ; Let C-k delete the whole line
  ad-redefinition-action 'accept                   ; Silence warnings for redefinition
  cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
  display-time-default-load-average nil            ; Don't display load average
  fill-column 80                                   ; Set width for automatic line breaks
  help-window-select t                             ; Focus new help windows when opened
  indent-tabs-mode nil                             ; Prefers spaces over tabs
  inhibit-startup-screen t                         ; Disable start-up screen
  initial-scratch-message ";; Happy Hacking!!!\n\n"; Empty the initial *scratch* buffer
  kill-ring-max 128                                ; Maximum length of kill ring
  load-prefer-newer t                              ; Prefers the newest version of a file
  mark-ring-max 128                                ; Maximum length of mark ring
  read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
  message-log-max 10000                            ; Maximum message log entries
  scroll-conservatively most-positive-fixnum       ; Always scroll by one line
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  tab-width 4                                      ; Set width for tabs
  ;; tab-always-indent 'complete                     ; make tab key do indent first then completion.
  user-full-name "Napoleon Ahiable"                ; Set the full name of the current user
  user-mail-address "219075+thepreacher@users.noreply.github.com" ; Set the email address of the current user
  vc-follow-symlinks t                             ; Always follow the symlinks
  next-line-add-newlines t                         ; insert newlines if the point is at the end of the buffer
  view-read-only t                                 ; Always open read-only buffers in view-mode
  cursor-type 'bar                                 ; Default is 'box (options - 'bar ,'hollow)
  require-final-newline t                          ; Newline at end of file
  column-number-mode 1)                            ; Show the column number




;; Customize default emacs file save locations
(setq-default
  auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t))
  auto-save-list-file-name `((".*" ,(expand-file-name "auto-save/auto-save-list" user-emacs-directory) t))
  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


;; General Customizations
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n

;; make typing delete/overwrites selected text
(add-hook 'after-init-hook 'delete-selection-mode)
(setq shift-select-mode nil)


;; Keybindings
(use-package use-package-chords
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)  ; 0.05 or 0.1
  (setq key-chord-one-key-delay 0.2))  ; 0.2 or 0.3 to avoid first autorepeat


;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; MacOS Specific keybindings
(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                     ((shift) . 5)
                                     ((control)))))



;;; Meow - https://github.com/DogLooksGood/meow
;;; Modalka - https://github.com/mrkkrp/modalka


(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; Super-save auto-saves your buffers, when certain events happen
(use-package super-save
  :init
  (add-hook 'after-init-hook 'super-save-mode t)
  :config
  (setq super-save-idle-duration 5)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-exclude '(".gpg"))
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))


;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)

;; Crux
(use-package crux
  :bind(("C-k" . crux-smart-kill-line)
        ("M-o" . crux-smart-open-line)
        ("C-c n" . crux-cleanup-buffer-or-region)
        ("C-c e" . crux-eval-and-replace)
        ("C-c D" . crux-delete-file-and-buffer)
        ("C-c d" . crux-duplicate-current-line-or-region)
        ("C-c r" . crux-rename-file-and-buffer)
        ("C-c f" . crux-recentf-find-file)
        ([remap kill-whole-line] . crux-kill-whole-line)
        ([remap move-beginning-of-line] . crux-move-beginning-of-line)
        ("C-<backspace>" . crux-kill-line-backwards)))


(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

;; Page break lines
(use-package page-break-lines
  :init (add-hook 'after-init-hook 'global-page-break-lines-mode t))




;;; Fonts and Themes

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Fonts
(defvar nqa/default-font-size 150)
(defvar nqa/default-variable-font-size 150)

(set-face-attribute
  'default nil
  :font "JetBrainsMono Nerd Font"
  :height nqa/default-font-size)
;; Set the fixed pitch face
(set-face-attribute
  'fixed-pitch nil
  :font "JetBrainsMono Nerd Font"
  :height nqa/default-font-size)
;; Set the variable pitch face
(set-face-attribute
  'variable-pitch nil
  :font "SF Pro Display"
  :height nqa/default-variable-font-size
  :weight 'regular)


;; Icons
(use-package all-the-icons
  :defer
  :if (display-graphic-p)
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))


;; Doom
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Doom-modeline
(use-package doom-modeline
  :init
  (add-hook 'after-init-hook 'doom-modeline-init t)
  (setq doom-modeline-height 15)
  (setq doom-modeline-lsp t))


;; Move cursor by camelCase
(global-subword-mode 1)


;; Visual-fill-column
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

;; Huge files
(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))


;; Move text
;; Moves the current line (or if marked, the current region’s, whole lines).
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; Aggressive-indent
(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))

;; Paren Faces
(use-package faces
  :ensure nil
  :init
  ;; don't delay show parens immediately
  (setq show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#ffb86c") ;; #262b36 previous
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))

;; Parens
(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (show-paren-mode +1))

;; Smartparens
;; (use-package smartparens
;;   :hook (after-init . smartparens-global-mode)
;;   :delight
;;   :config
;;   (require 'smartparens-config))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; highlight the current line
(use-package hl-line
  :init
  (add-hook 'after-init-hook 'global-hl-line-mode t))


;; Highlight indentation
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

(use-package highlight-symbol
  :delight
  :hook (prog-mode . highlight-symbol-mode)
  :custom-face
  (highlight-symbol-face ((t (:background "#44475a"))))
  :custom
  (highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :hook (prog-mode . highlight-operators-mode))

(use-package highlight-escape-sequences
    :hook (prog-mode . hes-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :hook (prog-mode . volatile-highlights-mode))

(cua-selection-mode t) ;; cua goodness without copy/paste etc.

;; Electric pair
(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; electric-operator
;; electric-operator is an emacs minor-mode to automatically add spacing around operators.
(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

;; Undo
(use-package undo-tree
  :delight
  :bind ("C--" . undo-tree-redo)
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

;; Auto load modified fils on
(use-package autorevert
  :init
  (add-hook 'after-init-hook 'global-auto-revert-mode t)
  :ensure nil
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :init (setq auto-revert-verbose nil))

;; Improved GNU Emacs standard package menu
(use-package paradox
  :defer
  :init
  (setq paradox-column-width-package 27
        paradox-column-width-version 13
        paradox-execute-asynchronously t
        paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; History
(use-package savehist
  :ensure nil
  :unless noninteractive
  :init
  (add-hook 'after-init-hook 'savehist-mode t)
  (setq history-delete-duplicates t
        history-length t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-file (expand-file-name "var/history" user-emacs-directory)
        savehist-save-minibuffer-history 1))


;; Abbreviations
(use-package abbrev
  :ensure nil
  :delight
  :hook (text-mode . abbrev-mode)
  :init
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; EditorConfig
(use-package editorconfig
  :init
  (add-hook 'after-init-hook 'editorconfig-mode t))


;; Windows

;; Don’t ask before killing a buffer. I know what I’m doing.
(global-set-key [remap kill-buffer] #'kill-this-buffer)

;; Give focus to newly created windows
(use-package window
  :ensure nil
  :bind (("C-x 3" . hsplit-last-buffer)
         ("C-x 2" . vsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Gives the focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Gives the focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

;; switch-window (alternative is ace-window)
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

;; windmove - window config management
;; By default, this one is bound to the key sequence ctrl-c left. If you change
;; your mind (while undoing), you can press ctrl-c right (calling winner-redo)
(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

;; winner
(use-package winner
  :init
  (add-hook 'after-init-hook 'winner-mode t))


;;; Directory and Files
(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; Dired
(use-package dired
  :ensure nil
  :init
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t)
  ;; auto refresh dired when file changes
  :hook (dired-mode . auto-revert-mode)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :init
  (setq dired-listing-switches "-agho --group-directories-first")
  :config
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))


(use-package dired-single
  :defer)

(use-package diredfl
  :hook (dired-mode . diredfl-global-mode)
  :config
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-open
  :defer
  :init
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;;preview files in dired
(use-package peep-dired
  :defer ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Tree style directory views in dired
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))


;; Tramp
(use-package tramp
  :defer
  :config
  ;; jww (2018-02-20): Without this change, tramp ends up sending hundreds of
  ;; shell commands to the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory `(("." . ,(concat user-emacs-directory "backups")))
        tramp-persistency-file-name "data/tramp"))

;; Saveplace
(use-package saveplace
  :unless noninteractive
  :init
  (setq save-place-file (expand-file-name "var/places" user-emacs-directory))
  :config
  (save-place-mode +1))

;; Uniquify
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Spelling
(require 'ispell)

(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-program-name "/usr/local/bin/aspell"))


;; Treemacs - tree layout file explorer
(use-package treemacs
  :defer
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (concat user-emacs-directory "/var/treemacs-persist")
          treemacs-position                      'left
          treemacs-position                      'right
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 20)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
         (`(t . t)
          (treemacs-git-mode 'deferred))
         (`(t . _)
          (treemacs-git-mode 'simple))))
  :bind ((:map global-map
           ("M-0"       . treemacs-select-window)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t B"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag))))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after (treemacs lsp-mode))


;; Alert
(use-package alert
  :config (setq alert-default-style 'libnotify))

;;Word Wrap
;; I like to have lines of the same length.

(use-package simple
  :ensure nil
  :delight (auto-fill-function)
  :bind ("C-x p" . pop-to-mark-command)
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :config (setq set-mark-command-repeat-pop t))


;;; Help and Discoverability

;; Which key
(use-package which-key
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.4))


;; Helpful - A better emacs help buffer
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))


;; Projectile - Project Intereaction Library
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (add-hook 'after-init-hook 'projectile-mode t)
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
      (setq projectile-project-search-path '("~/projects/learn/elixir/"
                                              "~/projects/learn/python/"
                                              "~/projects/learn/elm/"
                                              "~/projects/learn/rust/"
                                              "~/projects/learn/ai/"
                                              "~/projects/learn/c++/"
                                              "~/projects/learn/js/"
                                              "~/projects/probono/"
                                              "~/projects/paid/")))
  :bind-keymap (("C-x p" . projectile-command-map))
  :config
  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  ;; https://docs.projectile.mx/projectile/configuration.html#projectile-commander
  (setq projectile-switch-project-action 'projectile-commander)

  (def-projectile-commander-method ?\C-m
    "Find file in project."
    (call-interactively #'projectile-find-file))

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)
  (setq projectile-known-projects-file (expand-file-name "var/projectile-bookmarks.eld" user-emacs-directory)))

;; Bufler
(use-package bufler
  :defer
  :init
  (global-set-key (kbd "C-x C-b") 'bufler))



;;; Dev Utils

;; Linters
(use-package flycheck
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-delay .3
        flycheck-pylintrc "~/.pylintrc"
        flycheck-python-pylint-executable "/usr/bin/pylint"
        flycheck-stylelintrc "~/.stylelintrc.json")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))


;;YASnippet
(use-package yasnippet
  :defer 1
  :delight yas-minor-mode " υ"
  :hook ((yas-minor-mode . my/disable-yas-if-no-snippets))
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1)))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; Loren ipsum
(use-package lorem-ipsum
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))


;; Version control
;; Git
(use-package git-commit
  :after magit
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :init (setq git-commit-summary-max-length 50)
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

(use-package magit
  :commands (magit-git-repo-p magit-status magit-get-current-branch)
  :bind (("C-c g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Add a super-convenient global binding for magit-status
(global-set-key (kbd "C-M-;") 'magit-status)

(use-package smerge-mode
  :after hydra
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (hydra-merge/body)))))

;; In addition to that, I like to see the lines that are being modified in the file while it is being edited.
(use-package git-gutter
  :defer 0.3
  :delight
  :init (global-git-gutter-mode +1))

;; Finally, one last package that I like to use with Git to easily see the changes made by previous commits.
(use-package git-timemachine
  :defer 1
  :delight)


;; Selectrum is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :after orderless
  :defer 0.5
  ;; :init
  ;; (add-hook 'after-init-hook 'selectrum-mode t)
  :config
  (selectrum-mode)
  ;; Prescient is a library for intelligent sorting and
  ;; filtering in various contexts.
  (use-package prescient
    :config
    ;; Remember usage statistics across Emacs sessions.
    (prescient-persist-mode +1)
    ;; The default settings seem a little forgetful to me. Let's try
    ;; this out.
    (setq prescient-history-length 1000))

  ;; Selectrum-prescient provides intelligent sorting and
  ;; filtering for candidates in Selectrum menus.
  (use-package selectrum-prescient
    :config
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
    (selectrum-prescient-mode +1))

  ;; Like Counsel for Selectrum
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (("C-x M-:" . consult-complex-command)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-x b" . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x 5 b" . consult-buffer-other-frame)
           ("C-x r x" . consult-register)
           ("C-x r b" . consult-bookmark)
           ("M-g g" . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
           ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
           ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
           ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
           ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
           ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
           ("M-g i" . consult-project-imenu) ;; or consult-imenu
           ("M-g e" . consult-error)
           ;; ("M-s m" . consult-multi-occur)
           ("M-y" . consult-yank-pop)
           ("<help> a" . consult-apropos))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Custom command wrappers. It is generally encouraged to write your own
    ;; commands based on the Consult commands. Some commands have arguments which
    ;; allow tweaking. Furthermore global configuration variables can be set
    ;; locally in a let-binding.
    (defun my-fdfind (&optional dir)
      (interactive "P")
      (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
        (consult-find dir)))

    ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
    (fset 'multi-occur #'consult-multi-occur)

    ;; Configure register preview function.
    ;; This gives a consistent display for both `consult-register' and
    ;; the register preview when editing registers.
    (setq register-preview-delay 0
          register-preview-function #'consult-register-preview)

    ;; Configure other variables and modes in the :config section, after lazily loading the package
    :config

    ;; Configure preview. Note that the preview-key can also be configured on a
    ;; per-command basis via `consult-config'.
    ;; The default value is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-p"))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

    ;; Optionally configure narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")
    ;; Optionally make narrowing help available in the minibuffer.
    ;; Probably not needed if you are using which-key.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Optional configure a view library to be used by `consult-buffer'.
    ;; The view library must provide two functions, one to open the view by name,
    ;; and one function which must return a list of views as strings.
    ;; Example: https://github.com/minad/bookmark-view/
    ;; (setq consult-view-open-function #'bookmark-jump
    ;;       consult-view-list-function #'bookmark-view-names)

    ;; Optionally configure a function which returns the project root directory
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-root-function #'projectile-project-root))


  ;; Optionally add the `consult-flycheck' command.
  (use-package consult-flycheck
    :bind (:map flycheck-command-map
                ("!" . consult-flycheck)))


  (use-package embark
    :init
    (setq embark-action-indicator
      (lambda (map)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
    :bind ("C-," . embark-act)
    :config
    ;; For Selectrum users:
    (defun current-candidate+category ()
      (when selectrum-active-p
        (cons (selectrum--get-meta 'category)
              (selectrum-get-current-candidate))))

    (add-hook 'embark-target-finders #'current-candidate+category)

    (defun current-candidates+category ()
      (when selectrum-active-p
        (cons (selectrum--get-meta 'category)
              (selectrum-get-current-candidates
               ;; Pass relative file names for dired.
               minibuffer-completing-file-name))))

    (add-hook 'embark-candidate-collectors #'current-candidates+category)

    ;; No unnecessary computation delay after injection.
    (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

    (defun refresh-selectrum ()
      (setq selectrum--previous-input-string nil))

    (add-hook 'embark-pre-action-hook #'refresh-selectrum))


  ;; Like Ivy Rich for Selectrum
  ;; Enable richer annotations using the Marginalia package
  (use-package marginalia
    :after embark
    :bind (:map minibuffer-local-map
            ("C-M-a" . marginalia-cycle)
           ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
           :map embark-general-map
            ("A" . marginalia-cycle))


    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode)

    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

    ;; Prefer richer, more heavy, annotations over the lighter default variant.
    ;; E.g. M-x will show the documentation string additional to the keybinding.
    ;; By default only the keybinding is shown as annotation.
    ;; Note that there is the command `marginalia-cycle' to
    ;; switch between the annotators.
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))))




;; Efficient solution for single-buffer text search like Swiper
(use-package ctrlf
  :init
  (add-hook 'after-init-hook 'ctrlf-mode t))



;; Avy move aroung buffer by search
(use-package avy
  :bind ("C-;" . avy-goto-word-or-subword-1)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))



;; Recent Files
(use-package recentf
  :init
  (add-hook 'after-init-hook 'recentf-mode t)
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 50
        recentf-max-menu-items 15
        recentf-save-file (concat user-emacs-directory "var/recentf")
        recentf-exclude (list "COMMIT_EDITMSG"
                              "~$"
                              "/scp:"
                              "/ssh:"
                              "/sudo:"
                              "/tmp/"
                              "/\\.git/.*\\'"
                              "/elpa/.*\\'"
                              "/tramp.*\\'"
                              "/sudo.*\\'"
                              "/node_modules/.*\\'")))


;; Incremental parsing library
(use-package tree-sitter
  :defer 0.5
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))




;;; Code completion

;; Company - Auto completion system
;; smart tab behavior - indent or complete
;; (setq tab-always-indent 'complete)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; (add-to-list 'completion-styles 'initials t)

(use-package orderless
  :custom
  (completion-styles '(orderless))
  :config
  (setq orderless-component-separator " +"))

(use-package company
  :after orderless
  :init
  (add-hook 'after-init-hook 'global-company-mode t)
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
  :bind (:map company-active-map
          ("RET" . nil)
          ("[return]" . nil)
          ("TAB" . company-complete-common)
          ("<tab>" . company-complete-common)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))
  :config
  ;; Make completions display twice as soon.
  (setq company-idle-delay 0)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 3)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t))

(use-package company-prescient
  :after company
  :config
  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1)


  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (with-eval-after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Point and Region
;; Increase region by semantic units. It tries to be smart about it and adapt to the structure of the current major mode.
;; If you expand too far, you can contract the region by pressing - (minus key),
;; or by prefixing the shortcut you defined with a negative argument: C-- C-=.
(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :bind (("C-=" . er/expand-region)))



;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((java-mode
          python-mode
          go-mode
          js-mode
          js2-mode
          typescript-mode
          web-mode
          c-mode
          cpp-mode
          objc-mode
          elixir-mode
          elm-mode
          rust-mode
          haskell-literate-mode
          haskell-mode) . lsp-deferred)
        (lsp-mode . lsp-enable-which-key-integration)
        (lsp-mode . (lambda ()
                      (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  :init
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-print-io nil ;; Set to t when debugging lsp errors
        lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")
        lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-lens-enable nil
        lsp-idle-delay 0.5
        lsp-session-file (expand-file-name "var/.lsp-session-v1" user-emacs-directory)
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :workspace
        lsp-completion-provider :capf
        lsp-restart 'auto-restart
        lsp-eldoc-enable-hover t ;; Eldoc
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers t
        lsp-enable-semantic-highlighting t
        lsp-headerline-breadcrumb-enable nil ;; Don't have any for this now.
        lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  ;; file watcher directories to ignore
  (defvar nqa/ignored-directories '("[/\\\\]\\.elixir_ls\\'" ;; elixir lang server directory
                                    "[/\\\\]_build\\'" ;; elixir build directory
                                    "[/\\\\]assets\\'" ;; elixir assets directory
                                    "[/\\\\]deps" ;; elixir deps directory
                                    "[/\\\\]elm-stuff\\'")) ;; elm packages directory

  (dolist (re nqa/ignored-directories)
    (push re lsp-file-watch-ignored-directories))

  ;; don't scan 3rd party javascript libraries
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored-files)) ; json


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t ;; * Show only hover symbols
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'bottom))

(use-package dap-mode
  :after lsp-mode
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (dap-mode)
                                 (dap-ui-mode))))



;;; Languages

;; Elm
(use-package elm-mode
  :mode ("\\.elm\\'"))


;; Elxir
;; https://github.com/elixir-lsp/elixir-ls
(defvar lsp-elixir--config-options (make-hash-table))

(use-package elixir-mode
  :mode ("\\.exs?\\'")
  ;; :init
  ;; ;; for executable of language server, if it's not symlinked on your PATH
  ;; (add-to-list 'exec-path (expand-file-name "~/lang-servers/elixir-ls/release/")) ;; Uncomment for lsp-mode
  :config
  (use-package flycheck-credo
    :init
    (setq flycheck-elixir-credo-strict t)
    :config
    (flycheck-credo-setup)

    (add-hook 'lsp-after-initialize-hook
        (lambda ()
          (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))))


;; C / C++
(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode))
  :init
  (add-to-list 'exec-path (expand-file-name "/usr/local/opt/llvm/bin/")) ;; Uncomment for clangd
  :ensure-system-package clangd)

(use-package google-c-style
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;; CMake

;; CMake is a cross-platform build system generator.
(use-package cmake-mode
  :defer
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :after (cmake-mode)
  :hook (c++-mode . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
  :init (cmake-ide-setup)
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

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

;; Dart

;; In order to have a fast and stable environment, I recommend using LSP as a client for LSP servers and dart_language_server as server.
;; To use dart_language_server with GNU Emacs, you must first install it with the package manager of your operating system.
(use-package dart-mode
  :mode "\\.dart\\'"
  :init
  (setq dart-format-on-save t
        dart-sdk-path "~/sdks/flutter/bin/")
  :config
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-c" . flutter-run-or-hot-reload))
  :init (setq flutter-sdk-path "~/sdks/flutter/bin/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :config (flutter-l10n-flycheck-setup))

;; Docker

;; I like to use Docker when I need to install various databases or other services that only work on a particular operating system while keeping my operating system clean.

(use-package dockerfile-mode
  :delight "δ "
  :mode "Dockerfile\\'")

;; Emacs Lisp

(use-package elisp-mode
  :ensure nil
  :delight "ξ ")

;; Eldoc

;; Provides minibuffer hints when working with Emacs Lisp.
(use-package eldoc
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

;; HTML

;; In order to have a fast and stable environment, I recommend using LSP as a client for LSP servers and vscode-html-languageserver as server.
;; To use vscode-html-languageserver with GNU Emacs, you must first install it with the package manager of your operating system.

(use-package emmet-mode
  :delight
  :hook (css-mode sgml-mode web-mode))

;; INI

;; ini-mode does a good job of handling .ini files.
(use-package ini-mode
  :mode ("\\.ini\\'"))

;;Java

;; In order to have a fast and stable environment, I recommend using lsp-java as LSP client and Eclipse JDT Language Server as LSP server.
;; NOTE: before configuring lsp-java, don’t forget to configure lsp-mode.
(use-package lsp-java
  :after lsp)

;; Gradle
(use-package gradle-mode
  :mode ("\\.java\\'" "\\.gradle\\'")
  :bind (:map gradle-mode-map
          ("C-c C-c" . gradle-build)
          ("C-c C-t" . gradle-test))
  :preface
  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :config
  (advice-add 'gradle-build :after #'my/switch-to-compilation-window)
  (advice-add 'gradle-test :after #'my/switch-to-compilation-window))


;; JavaScript

;; For my JavaScript configuration, I took my sources from the Nicolas Petton’s blog which I found very well explained.
;; Setting up Emacs for JavaScript (part #1) Setting up Emacs for JavaScript (part #2)
;; I like to use prettier to get my TypeScript code clean. To use it, don’t forget to install it with your package manager.
(use-package prettier-js
  :defer
  :custom (prettier-js-args '("--print-width" "100"
                              "--single-quote" "true"
                              "--trailing-comma" "all")))

;; js2-mode

;; By default, GNU Emacs uses js-mode as major mode for JavaScript buffers and I
;; prefer use js2-mode instead because of his abilities to parses buffers and builds an AST for things like syntax highlighting.
(use-package js2-mode
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . prettier-js-mode))
  :mode "\\.js\\'"
  :init (setq js-indent-level 2))

;; js2-refactor

;; Provides powerful refactoring based on the AST generated by js2-mode.
(use-package js2-refactor
  :bind (:map js2-mode-map
          ("C-k" . js2r-kill)
          ("M-." . nil))
  :hook ((js2-mode . js2-refactor-mode)
         (js2-mode . (lambda ()
                       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  :init (setq js2r-add-keybindings-with-prefix "C-c C-r"))

;; xref-js2

;; Makes it easy to jump to function references or definitions.
(use-package xref-js2 :defer 5)

;; tern

;; Parses JavaScript files in a project and makes type inference to provide meaningful completion (with type clues) and cross-reference support.
;; Unfortunately, tern has some problems with cross-references that explain why I am using xref-js2 instead.

(use-package tern
  :ensure-system-package (tern . "pnpm add -g tern")
  :bind (("C-c C-c" . compile)
         :map tern-mode-keymap
         ("M-." . nil))
  :hook ((js2-mode . company-mode)
         (js2-mode . tern-mode)))

;; Then, add a .tern-project file to the root of your project.
;; Here is an example configuration for a project that uses requirejs and jQuery, without taking into account of the bower_components directory:
;;
;; {
;;     "libs": [
;;         "jquery"
;;     ],
;;     "loadEagerly": [
;;         "./**/*.js"
;;     ],
;;     "dontLoad": [
;;         "./bower_components/"
;;     ],
;;     "plugins": {
;;         "requirejs": {
;;             "baseURL": "./"
;;         }
;;     }
;; }
;;


;; JSON

;; JSON is used a lot, especially in the web. Therefore, it is important to have a decent configuration to feel comfortable when handling such files.
(use-package json-mode
  :delight "J "
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Prints the arrays of numbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line))


;; Toml
(use-package toml-mode
  :mode ("\\.toml\\'"))

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'"))


;; Go
(use-package go-mode
  :mode ("\\.go$"))

;; Haskell
(use-package haskell-mode
  :mode ("\\.l?hs\\'")
  :hook (haskell-mode . turn-on-haskell-indentation)
  :config
  (use-package lsp-haskell))

;; LaTeX

;; I use LaTeX for my reports, CVs, summaries, etc.
(use-package tex
  :ensure auctex
  :bind (:map TeX-mode-map
          ("C-c C-o" . TeX-recenter-output-buffer)
          ("C-c C-l" . TeX-next-error)
          ("M-[" . outline-previous-heading)
          ("M-]" . outline-next-heading))
  :hook (LaTeX-mode . reftex-mode)
  :preface
  (defun my/switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX Help* buffer after compilation."
    (other-window 1))
  :init
  (setq TeX-auto-save t
        TeX-byte-compile t
        TeX-clean-confirm nil
        TeX-master 'dwim
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (advice-add 'TeX-next-error :after #'my/switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'my/switch-to-help-window)
  ;; the ":hook" doesn't work for this one... don't ask me why.
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer))

(use-package bibtex
  :after auctex
  :hook (bibtex-mode . my/bibtex-fill-column)
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math :after (auctex company))

;; I want a TeX engine that can deal with Unicode and use any font I like.

(setq-default TeX-engine 'xetex)

;; reftex
;; Minor mode with distinct support for \label, \ref and \cite in LaTeX.
(use-package reftex
  :after auctex
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t))

;; Lua
(use-package lua-mode
  :delight "Λ "
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

;; Markdown
(use-package markdown-mode
  :ensure-system-package (pandoc)
  :delight "μ "
  :mode ("\\.markdown\\'" "\\.md\\'")
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(use-package markdown-preview-mode
  :after markdown-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))



;; PHP

;; https://github.com/felixfbecker/php-language-server
;; For people who wonder, I don’t use php-mode because it can’t handle files that
;; contain PHP and HTML. Also, why use another package when web-mode already provides everything I need?
;; The function below provides my own PHP configuration with flycheck.

(defun my/php-setup ()
  (web-mode)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset))

;; Don’t forget to add the following line in the web-mode package configuration:

;; (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))

;; I like to use ac-php to enable GNU Emacs auto-completion for PHP.
;; **NOTE:** ac-php supports company mode and auto-complete.

(use-package ac-php
  :after (company php-mode)
  :hook (php-mode . ac-php-mode)
  :custom (ac-sources '(ac-source-php))
  :config
  (ac-php-core-eldoc-setup)
  (auto-complete-mode t))


;; PlantUML
(use-package plantuml-mode
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :init (setq plantuml-jar-path (expand-file-name "~/.local/lib/plantuml.jar")))


;; Python
(use-package blacken
  :delight
  :hook (python-mode . blacken-mode)
  :init (setq blacken-line-length 79))

;; Pipenv Environment management
(use-package pipenv
  :hook (python-mode . pipenv-mode))


(use-package lsp-pyright
  :after lsp-mode
  :if (executable-find "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))

(use-package python
  :mode "\\.py\\'"
  :delight "π "
  :bind (("M-[" . python-nav-backward-block)
         ("M-]" . python-nav-forward-block))
  :preface
  (defun python-remove-unused-imports()
    "Removes unused imports and unused variables with autoflake."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (warn "python-mode: Cannot find autoflake executable."))))

(use-package py-isort
  :after python
  :hook ((python-mode . pyvenv-mode)
         (before-save . py-isort-before-save)))

(use-package pyenv-mode
  :after python
  :hook ((python-mode . pyenv-mode)
         (projectile-switch-project . projectile-pyenv-mode-set))
  :init (setq pyenv-mode-set "3.9.1")
  :preface
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(use-package pyvenv
  :after python
  :hook ((python-mode . pyvenv-mode)
         (python-mode . (lambda ()
                          (if-let ((pyvenv-directory (find-pyvenv-directory (buffer-file-name))))
                              (pyvenv-activate pyvenv-directory)))))
  :init
  ;; (setq pyvenv-default-virtual-env-name "env")
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:"
                                                              pyvenv-virtual-env-name "]")))
  :preface
  (defun find-pyvenv-directory (path)
    "Checks if a pyvenv directory exists."
    (cond
     ((not path) nil)
     ((file-regular-p path) (find-pyvenv-directory (file-name-directory path)))
     ((file-directory-p path)
      (or
       (seq-find
        (lambda (path) (file-regular-p (expand-file-name "pyvenv.cfg" path)))
        (directory-files path t))
       (let ((parent (file-name-directory (directory-file-name path))))
         (unless (equal parent path) (find-pyvenv-directory parent))))))))


;; Shell Script
(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))


;; SQL
(use-package sql-indent
  :after (:any sql sql-interactive-mode)
  :delight sql-mode "Σ ")


;; Typescript
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-mode . prettier-js-mode)
  :custom
  (add-hook 'typescript-mode-hook #'(lambda ()
                                      (enable-minor-mode
                                       '("\\.tsx?\\'" . prettier-js-mode)))))

;; Vue.js
(use-package vue-mode
  :delight "V "
  :mode "\\.vue\\'"
  :init
  (setq mmm-submode-decoration-level 0
        vue-html-extra-indent 2))

;; XML
(use-package xml-mode
  :ensure nil
  :mode ("\\.wsdl\\'" "\\.xsd\\'"))

;; Yaml
(use-package yaml-mode
  :delight "ψ "
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode))


;; Web-mode
;; An autonomous emacs major-mode for editing web templates.
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

;; Elixir plus Liveview templates
(add-to-list 'auto-mode-alist
             '("\\.l?eex\\'" . (lambda ()
                               ;; add major mode setting here, if needed, for example:
                               ;; (text-mode)
                                (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

(use-package web-mode
  :delight "☸ "
  :hook ((css-mode web-mode) . rainbow-mode)
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.l?eex\\'" . web-mode)
         ("\\.php$" . my/php-setup))
  :preface
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  :init
  (setq web-mode-code-indent-offset                   2
        web-mode-markup-indent-offset                 2
        web-mode-css-indent-offset                    2
        web-mode-markup-indent-offset                 2
        web-mode-code-indent-offset                   2
        web-mode-comment-style                        2
        web-mode-enable-block-face                    nil
        web-mode-enable-comment-annotation            t
        web-mode-enable-css-colorization              t
        web-mode-enable-current-column-highlight      t
        web-mode-enable-current-element-highlight     t
        web-mode-enable-inlays                        nil
        web-mode-enable-optional-tags                 t
        web-mode-enable-part-face                     t
        web-mode-enable-sexp-functions                nil
        web-mode-enable-sql-detection                 nil
        web-mode-enable-string-interpolation          t
        web-mode-enable-comment-interpolation         t
        web-mode-enable-heredoc-fontification         nil
        web-mode-enable-html-entities-fontification   nil
        web-mode-enable-element-content-fontification nil
        web-mode-enable-whitespace-fontification      nil
        web-mode-enable-auto-expanding                t
        web-mode-enable-control-block-indentation     t
        web-mode-enable-auto-indentation              t
        web-mode-enable-auto-closing                  nil
        web-mode-enable-auto-opening                  nil
        web-mode-enable-auto-pairing                  t
        web-mode-enable-auto-quoting                  t))


(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.js?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.ts?\\'" . prettier-js-mode))))





;;; PDF

(use-package pdf-tools
  :init
  (add-hook 'after-init-hook 'pdf-tools-install)
  :magic ("%PDF" . pdf-view-mode)
  :init
  ;; this only has to be executed for the installation and can be removed/commented afterwards
  ;; I recommend commenting it out so that it can be found easily when reinstalling
  (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig")
  (add-hook 'pdf-isearch-minor-mode-hook (lambda () (ctrlf-local-mode -1))))
  ;; (pdf-tools-install :no-query))

(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
          ("C-s" . isearch-forward)
          ("d" . pdf-annot-delete)
          ("h" . pdf-annot-add-highlight-markup-annotation)
          ("t" . pdf-annot-add-text-annotation))
  :init
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1
        pdf-view-use-unicode-ligther nil))



;; Dashboard
;; (use-package dashboard
;;   :if (< (length command-line-args) 2)
;;   :preface
;;   (defun dashboard-load-packages (list-size)
;;     (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 5) ?)
;;             (format "%d packages loaded in %s" (length package-activated-list) (emacs-init-time))))
;;   :init
;;   (add-hook 'after-init-hook 'dashboard-setup-startup-hook t)
;;   (progn
;;     (setq dashboard-items '((recents . 5)
;;                             (projects . 10)))
;;     (setq dashboard-banner-logo-title "Happy Coding!!!")
;;     (setq dashboard-startup-banner "~/Documents/napo_avatar_rounded.png")
;;     (setq dashboard-set-file-icons t)
;;     (setq dashboard-set-heading-icons t)
;;     (setq dashboard-set-footer nil))
;;   :config
;;   (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages)))


;;; Misc

(add-hook 'after-init-hook 'transient-mark-mode)


;; Try packages without installing
(use-package try :defer)

;;Dash - A modern list api for Emacs. No 'cl required.
(use-package dash :defer)
(use-package dash-functional :defer)

;; Show event history and command history of some or all buffers.
(use-package command-log-mode :defer)

;; Whitespaces
;;It is often annoying to see unnecessary blank spaces at the end of a line or file. Let’s get ride of them:
(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

;; hungry-delete
;; Deleting a whitespace character will delete all whitespace until the next non-whitespace character.
(use-package hungry-delete
  :init
  (add-hook 'after-init-hook 'global-hungry-delete-mode t))



;; Display line numbers
(setq-default display-line-numbers-type t ;; for relative numbering set this to 'visual or 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

;; This should be at the bottom for it to work
(set-face-attribute 'line-number nil
            :font "JetBrainsMono Nerd Font")

(set-face-attribute 'line-number-current-line nil
            :weight 'bold
            :font "JetBrainsMono Nerd Font"
            :foreground "goldenrod")

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


;; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/projects/"))

;;; Locales and UTF-8
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))



(provide 'init)
;; Local Variables:
;;; init.el ends here
