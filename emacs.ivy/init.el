;;; init.el --- GNU Emacs Configuration -*- lexical-binding: t -*-
;;; Author: Napleon Ahiable
;;; Commentary:
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org#about

;; Targeted for Emacs 27+

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;(setq debug-on-error t)

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

(add-hook 'emacs-startup-hook
    #'(lambda ()
        (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
             (float-time
               (time-subtract after-init-time before-init-time)))
            gcs-done)))

(require 'package)
(require 'cl-lib)

;; Package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux plaforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)


(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update))

(setq package-check-signature nil) ;; uncomment when having bad signature issues

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
  frame-resize-pixelwise t                         ;
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
  view-read-only t                                 ; Always open read-only buffers in view-mode
  cursor-type 'bar                                 ; Default is 'box (options - 'bar ,'hollow)
  column-number-mode 1                             ; Show the column number
  show-paren-mode 1)                               ; Show the parent



;; Customize default emacs file save locations
(setq-default
  auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t))
  auto-save-list-file-name `((".*" ,(expand-file-name "auto-save/auto-save-list" user-emacs-directory) t))
  backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


;; General Customizations

(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n

;; Keybindings

;; MacOS Specific keybindings
(when *sys/mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                     ((shift) . 5)
                                     ((control)))))

;; Super-save auto-saves your buffers, when certain events happen
(use-package super-save
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-idle-duration 30)
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
        ("C-M-o" . crux-smart-open-line-above)
        ("C-c f" . crux-recentf-find-file)
        ("C-c n" . crux-cleanup-buffer-or-region)
        ("C-c e" . crux-eval-and-replace)
        ("C-c D" . crux-delete-file-and-buffer)
        ("C-c d" . crux-duplicate-current-line-or-region)
        ("C-c r" . crux-rename-file-and-buffer)
        ([remap kill-whole-line] . crux-kill-whole-line)
        ([remap move-beginning-of-line] . crux-move-beginning-of-line)
        ("C-<backspace>" . crux-kill-line-backwards)))


;;; Fonts and Themes

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

;; Doom
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;; Doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode +1)
  (setq doom-modeline-height 15))

;; Icons
(use-package all-the-icons
  :defer
  :if (display-graphic-p)
  :init (unless (find-font (font-spec :name "all-the-icons"))
          (all-the-icons-install-fonts t)))

;; Faces
(use-package faces
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#262b36")
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))


;; move cursor by camelCase
(use-package subword-mode
  :ensure nil
  :hook (after-init . global-subword-mode))

;; visual-fill-column
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

;; move-text
;; Moves the current line (or if marked, the current region’s, whole lines).
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; aggressive-indent
(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))

(electric-pair-mode)

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
  :hook (after-init . global-auto-revert-mode)
  :ensure nil
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :init (setq auto-revert-verbose nil))

;; Improved GNU Emacs standard package menu
(use-package paradox
  :commands paradox-list-packages
  :init
  (setq paradox-column-width-package 27
        paradox-column-width-version 13
        paradox-execute-asynchronously t
        paradox-hide-wiki-packages t)
  :config
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; History
(use-package savehist
  :unless noninteractive
  :hook (after-init . savehist-mode)
  :ensure nil
  :init
  (setq history-delete-duplicates t
        history-length t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-file (expand-file-name "var/history" user-emacs-directory)
        savehist-save-minibuffer-history 1))


;; highlight the current line
(use-package hl-line
  :hook (after-init . global-hl-line-mode))

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


(cua-selection-mode t) ;; cua goodness without copy/paste etc.

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
  :hook (after-init . editorconfig-mode))


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
  :hook (after-init . winner-mode))


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


;; Treemacs
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


;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
        (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)


(use-package fullframe
  :after ibuffer
  :config
  (fullframe ibuffer ibuffer-quit))

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(use-package ibuffer-vc
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode .  ibuffer-set-up-preferred-filters))

;; Alert
(use-package alert
  :defer 1
  :init (setq alert-default-style 'libnotify))

;;Word Wrap
;; I like to have lines of the same length.

(use-package simple
  :ensure nil
  :delight (auto-fill-function)
  :bind ("C-x p" . pop-to-mark-command)
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :init (setq set-mark-command-repeat-pop t))


;;; Help and Discoverability

;; Which key
(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-idle-delay 0.4)
  (setq which-key-idle-secondary-delay 0.4)
  :delight)

;; Helpful - A better emacs help buffer
(use-package helpful
  :defer
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Projectile - Project discovery
(use-package projectile
  :defer
  :delight '(:eval (concat " " (projectile-project-name)))
  :preface
  (defun my/projectile-compilation-buffers (&optional project)
    "Get a list of a project's compilation buffers.
  If PROJECT is not specified the command acts on the current project."
    (let* ((project-root (or project (projectile-project-root)))
           (buffer-list (mapcar #'process-buffer compilation-in-progress))
           (all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (projectile-project-buffer-p buffer project-root))
                         buffer-list)))
      (if projectile-buffers-filter-function
          (funcall projectile-buffers-filter-function all-buffers)
        all-buffers)))
  :init
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
  (setq projectile-switch-project-action #'projectile-dired
        projectile-completion-system 'ivy
        projectile-known-projects-file (expand-file-name "var/projectile-bookmarks.eld" user-emacs-directory))
  :bind (:map projectile-mode-map
          ("C-c p" . projectile-command-map)))

;; Counsel integration for projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode +1))

;; Ibuffer
(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

;; Recentf
(use-package recentf
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 500
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


;; Hydra
(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ;; ("C-c L" . hydra-ledger/body)
         ("C-c M" . hydra-merge/body)
         ("C-c T" . hydra-tool/body)
         ("C-c b" . hydra-btoggle/body)
         ("C-c c" . hydra-clock/body)
         ;; ("C-c e" . hydra-erc/body)
         ("C-c f" . hydra-flycheck/body)
         ;; ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c s" . hydra-spelling/body)
         ("C-c t" . hydra-tex/body)
         ("C-c u" . hydra-upload/body)
         ("C-c w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))


;; Hydra / Flycheck
;; Group Flycheck commands.

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))


;; Hydra / Magit
;; Group Magit commands.

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

;; Hydra / Merge
;; Group Merge commands.

(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))

;; Hydra / Org
;; Group Org commands.

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

;; Hydra / Projectile
;; Group Projectile commands.

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))




;;; Dev Utils

;; Linters
(use-package flycheck
  :defer
  :init
  (global-flycheck-mode +1)
  (setq flycheck-display-errors-delay .3
        flycheck-pylintrc "~/.pylintrc"
        flycheck-python-pylint-executable "/usr/bin/pylint"
        flycheck-stylelintrc "~/.stylelintrc.json")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))


;;YASnippet
(use-package yasnippet
  :delight yas-minor-mode " υ"
  :defer
  :hook (;;(after-init . yas-global-mode)
         (yas-minor-mode . my/disable-yas-if-no-snippets))
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1)))
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))


;; Loren ipsum
(use-package lorem-ipsum
  :bind (("C-c C-v l" . lorem-ipsum-insert-list)
         ("C-c C-v p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-v s" . lorem-ipsum-insert-sentences)))


;; Avy move aroung buffer by search
(use-package avy
  :bind ("M-s" . avy-goto-char))



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
  :commands (magit-git-repo-p)
  :bind (("C-c g" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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


;; Ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-i" . ivy-partial-or-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :init
  (setq ivy-use-virtual-buffers t
        ;; abbreviate turns home into ~ (for example)
        ;; buffers still only get the buffer basename
        ivy-virtual-abbreviate 'abbreviate
        ivy-rich-path-style 'abbrev
        ;; ivy-initial-inputs-alist nil ;; Don't start searches with ^
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-magic-tilde nil
        ivy-dynamic-exhibit-delay-ms 100
        ivy-count-format "(%d/%d) "     ; Show current match and matches
        ivy-extra-directories nil)) ; Do not show "./" and "../"


;; Swiper
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

;; Counsel
(use-package counsel
  :after swiper
  :init
  (setq-default ivy-initial-inputs-alist
              '((Man-completion-table . "^")
                (woman . "^")))
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  :bind (("M-x" . counsel-M-x)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("M-?" . sanityinc/counsel-search-project)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
  ;; Use different regex strategies per completion command
  (setq ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (counsel-ag . ivy--regex-plus)
                                (counsel-projectile-ag . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (counsel-M-x . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (when (require 'projectile)
    (let ((search-function
           (cond
            ((executable-find "rg") 'counsel-rg)
            ((executable-find "ag") 'counsel-ag))))
      (when search-function
        (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
          "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
          If there is no project root, or if the prefix argument
          USE-CURRENT-DIR is set, then search from the current directory
          instead."
          (interactive (list (let ((sym (thing-at-point 'symbol)))
                               (when sym (regexp-quote sym)))
                             current-prefix-arg))
          (let ((current-prefix-arg)
                (dir (if use-current-dir
                         default-directory
                       (condition-case err
                           (projectile-project-root)
                         (error default-directory)))))))))))

;; Ivy rich
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))

  (defun ivy-rich-compiling (candidate)
    "Displays compiling buffers of the candidate for ivy-rich."
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
              (not (magit-git-repo-p candidate)))
          ""
        (if (my/projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))

  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate                (:width 73))
                (ivy-rich-file-user                (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group               (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes               (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size                (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time  (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate         (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon        (:width 2))
                (ivy-rich-candidate                 (:width 40))
                (ivy-rich-switch-buffer-size        (:width 7))
                (ivy-rich-switch-buffer-indicators  (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode  (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((counsel-M-x-transformer             (:width 40))
                (ivy-rich-counsel-function-docstring :face font-lock-doc-face)))))



(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :init
  (setq all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

;; Prescient
(use-package prescient
  :defer
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient counsel)
  :init
  (setq ivy-prescient-enable-filtering t)
  :config
  (ivy-prescient-mode +1))


(use-package ivy-xref :after ivy)

;; Incremental parsing library
(use-package tree-sitter
  :defer
  :config
  (use-package tree-sitter-langs))


;; Lsp
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
  (setq lsp-print-io nil ;; Set to t when debugging lsp errors
        lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")
        lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-lens-enable t
        lsp-idle-delay 0.1
        lsp-session-file (expand-file-name "var/.lsp-session-v1" user-emacs-directory)
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :workspace
        lsp-completion-provider :capf
        lsp-restart 'auto-restart
        lsp-enable-semantic-highlighting t
        lsp-headerline-breadcrumb-enable nil ;; Don't have any for this now.
        lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-completion-enable-additional-text-edit nil))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'bottom))

(use-package dap-mode
  :after lsp-mode
  :hook (after-init . (lambda ()
                        (dap-mode)
                        (dap-ui-mode))))



;;; Code completion

;; Company - Auto completion system
;; (add-to-list 'completion-styles 'initials t)

(use-package company
  :after lsp-mode
  :init
  (setq company-begin-commands '(self-insert-command)
        company-idle-delay 0
        company-minimum-prefix-length 3
        company-show-numbers t
        company-tooltip-align-annotations t)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode +1))

(use-package company-prescient
  :after company
  :config
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

;; Smartparens
(use-package smartparens
  :hook (after-init . smartparens-global-mode)
  :delight
  :init (setq sp-escape-quotes-after-insert nil)
  :config
  (sp-use-paredit-bindings))

;; Point and Region
;; Increase region by semantic units. It tries to be smart about it and adapt to the structure of the current major mode.
;; If you expand too far, you can contract the region by pressing - (minus key),
;; or by prefixing the shortcut you defined with a negative argument: C-- C-=.
(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))


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
  :hook ((c-mode c++-mode) . google-set-c-style)
        (c-mode-common . google-make-newline-indent))

;; CMake

;; CMake is a cross-platform build system generator.
(use-package cmake-mode
  :defer
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :after (projectile)
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
  :mode ("\\.rs\\'")
  :hook (before-save . (lambda ()
                          (when (eq 'rust-mode major-mode)
                              (lsp-format-buffer)))))

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
  (setq pyvenv-default-virtual-env-name "env")
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
  (add-hook 'typescript-mode-hook #'(lambda ())
                               (enable-minor-mode
                                '("\\.tsx?\\'" . prettier-js-mode))))

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
(use-package rainbow-mode :defer)

(use-package web-mode
  :delight "☸ "
  :hook ((css-mode web-mode) . rainbow-mode)
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.php$" . my/php-setup))
  :preface
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  :init
  (setq web-mode-attr-indent-offset 2
        web-mode-block-padding 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.js?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))

(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.ts?\\'" . prettier-js-mode))))


(setq web-mode-code-indent-offset                   2
      web-mode-markup-indent-offset                 2
      web-mode-css-indent-offset                    2
      web-mode-enable-html-entities-fontification   nil
      web-mode-enable-block-face                    nil
      web-mode-enable-comment-annotation            nil
      web-mode-enable-comment-interpolation         nil
      web-mode-enable-control-block-indentation     nil
      web-mode-enable-css-colorization              nil
      web-mode-enable-current-column-highlight      nil
      web-mode-enable-current-element-highlight     nil
      web-mode-enable-element-content-fontification nil
      web-mode-enable-heredoc-fontification         nil
      web-mode-enable-inlays                        nil
      web-mode-enable-optional-tags                 nil
      web-mode-enable-part-face                     nil
      web-mode-enable-sexp-functions                nil
      web-mode-enable-sql-detection                 nil
      web-mode-enable-string-interpolation          nil
      web-mode-enable-whitespace-fontification      nil
      web-mode-enable-auto-expanding                nil
      web-mode-enable-auto-indentation              nil
      web-mode-enable-auto-closing                  nil
      web-mode-enable-auto-opening                  nil
      web-mode-enable-auto-pairing                  nil
      web-mode-enable-auto-quoting                  nil)



;; Org-Mode

(defun nqa/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
        '(("^ *\\([-]\\) "
           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "SF Pro Display" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun nqa/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun nqa/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package org
  :pin org
  :hook ((after-init . org-mode)
         (org-mode . nqa/org-mode-setup)
         (org-mode . nqa/org-mode-visual-fill))
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
    '("~/Orgfiles/Tasks.org"
      "~/Orgfiles/Habits.org"
      "~/Orgfiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
      ;; Put mutually exclusive tags here
      (:endgroup)
      ("@errand" . ?E)
      ("@home" . ?H)
      ("@work" . ?W)
      ("agenda" . ?a)
      ("planning" . ?p)
      ("publish" . ?P)
      ("batch" . ?b)
      ("note" . ?n)
      ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
    '(("d" "Dashboard"
       ((agenda "" ((org-deadline-warning-days 7)))
        (todo "NEXT")
        ((org-agenda-overriding-header "Next Tasks"))
        (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

      ("n" "Next Tasks"
       ((todo "NEXT")
        ((org-agenda-overriding-header "Next Tasks"))))

      ("W" "Work Tasks" tags-todo "+work-email")

      ;; Low-effort next actions
      ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
       ((org-agenda-overriding-header "Low Effort Tasks")
        (org-agenda-max-todos 20)
        (org-agenda-files org-agenda-files)))

      ("w" "Workflow Status"
       ((todo "WAIT")
        ((org-agenda-overriding-header "Waiting on External")
         (org-agenda-files org-agenda-files))
        (todo "REVIEW")
        ((org-agenda-overriding-header "In Review")
         (org-agenda-files org-agenda-files))
        (todo "PLAN")
        ((org-agenda-overriding-header "In Planning")
         (org-agenda-todo-list-sublevels nil)
         (org-agenda-files org-agenda-files))
        (todo "BACKLOG")
        ((org-agenda-overriding-header "Project Backlog")
         (org-agenda-todo-list-sublevels nil)
         (org-agenda-files org-agenda-files))
        (todo "READY")
        ((org-agenda-overriding-header "Ready for Work")
         (org-agenda-files org-agenda-files))
        (todo "ACTIVE")
        ((org-agenda-overriding-header "Active Projects")
         (org-agenda-files org-agenda-files))
        (todo "COMPLETED")
        ((org-agenda-overriding-header "Completed Projects")
         (org-agenda-files org-agenda-files))
        (todo "CANC")
        ((org-agenda-overriding-header "Cancelled Projects")
         (org-agenda-files org-agenda-files))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Orgfiles/Tasks.org" "Inbox")
       "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
       (file+olp+datetree "~/Orgfiles/Journal.org")
       "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
       ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
       :clock-in :clock-resume
       :empty-lines 1)
      ("jm" "Meeting" entry
       (file+olp+datetree "~/Orgfiles/Journal.org")
       "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
       :clock-in :clock-resume
       :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Orgfiles/Journal.org")
       "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Orgfiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (nqa/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (python . t)))

;; (push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
;; (defun nqa/org-babel-tangle-config ()
;;   (when (string-equal (file-name-directory (buffer-file-name))
;;                       (expand-file-name user-emacs-directory))
;;     ;; Dynamic scoping to the rescue
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nqa/org-babel-tangle-config)))



;;; PDF

;;
;; (use-package pdf-tools
;;   :defer 1
;;   :magic ("%PDF" . pdf-view-mode)
;;   :init
;;   ;; this only has to be executed for the installation and can be removed/commented afterwards
;;   ;; I recommend commenting it out so that it can be found easily when reinstalling
;;   (setenv "PKG_CONFIG_PATH" "ccls/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
;;   (pdf-tools-install :no-query))
;;
;; (use-package pdf-view
;;   :ensure nil
;;   :after pdf-tools
;;   :bind (:map pdf-view-mode-map
;;           ("C-s" . isearch-forward)
;;           ("d" . pdf-annot-delete)
;;           ("h" . pdf-annot-add-highlight-markup-annotation)
;;           ("t" . pdf-annot-add-text-annotation))
;;   :init
;;   (setq pdf-view-display-size 'fit-page
;;         pdf-view-resize-factor 1.1
;;         pdf-view-use-unicode-ligther nil))



;;; Misc

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
  :hook (after-init . global-hungry-delete-mode)
  :delight)

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

;; Start emacs by default using the following directory
(setq default-directory (expand-file-name "~/projects/"))

(provide 'init)
;; Local Variables:
;;; init.el ends here
