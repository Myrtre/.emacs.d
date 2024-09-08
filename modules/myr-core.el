;;; package --- Core Settings -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Mail -----
(defvar myr/mail-enabled (member system-name '("jotenheim")))
(setq myr/mu4e-inbox-query nil)

;;; -- Basic Configuration -----
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file     (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :ensure nil
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; -- Native Compilation -----

(setq native-comp-async-report-warning-errors nil)
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; -- Basic Emacs Settings -----

(setq inhibit-startup-message t)

(unless myr/is-term
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-fringe-mode 10))

(menu-bar-mode -1)
(global-hl-line-mode 1)
(savehist-mode 1)
(show-paren-mode 1)

(setq-default fill-column 80)
(setq visible-bell t)
(unless myr/is-term
  (setq mouse-wheel-scrol-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mode 't)
  (setq scrool-step 1)
  (setq use-dialog-box nil))

(setq user-full-name "Davy Gút"
      user-mail-address "myrtre@proton.me"
      require-final-newline t
      message-send-mail-partially-limit nil
      kill-ring-max (* kill-ring-max 4)
      undo-strong-limit (* undo-strong-limit 4)
      undo-outer-limit (* undo-outer-limit 4)
      scroll-preserve-screen-position t
      mouse-yank-at-point t
      lazy-highlight-initial-delay 0.15
      save-interprogram-paste-before-kill t
      apropos-do-all t
      ;; choose browser
      browse-url-browser-function #'browser-url-generic
      browse-url-generic-program (if (eq system-type 'darwin) "open" "nyxt")
      frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name))))

(setq show-paren-context-when-offscreen 'child-frame)
(setq proced-enable-color-flag t)
(setq history-delete-duplicates t)
(setq help-window-select t)
(setq track-eol t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        last-kbd-macro
        kmacro-ring
        shell-command-history
        Info-history-list))

(when (>= emacs-major-version 27)
  (defun dotfiles--gc-on-last-frame-out-of-focus ()
    "GC if all frames are inactive"
    (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
        (garbage-collect)))
  (add-function :after after-focus-change-function
                #'dotfiles--gc-on-last-frame-out-of-focus))

(setq page-delimiter "^ *\C-l\n")

(setq enable-recursive-minibuffers t)


;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;; -- Core Keys and Packages
(repeat-mode 1)
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		        prog-mode-hook
		        conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq large-file-warning-threshold nil)
(setq vc-follow-symlink t)
(setq ad-redefinition-action 'accept)

;; Appearance
(use-package doom-themes
  :ensure nil
  :config
  (unless myr/is-term
    (load-theme
     (pcase system-name
       ("jotunheim" 'doom-gruvbox)
       (_ 'doom-palenight))
     t)
    (doom-themes-visual-bell-config)))

;; Set the font face based on platform
(set-face-attribute 'default nil
		            :font myr/default-font
		            :weight 'normal
		            :height (myr/system-settings-get 'emacs/default-face-size))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
		            :font myr/fixed-pitch-font
		            :weight 'normal
		            :height (myr/system-settings-get 'emacs/fixed-pitch-face-size))

(defvar myr/org-heading-font "Iosevka Aile")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
		            :font myr/varable-pitch-font
		            :weight 'normal
		            :height (myr/system-settings-get 'emacs/variable-pitch-face-size))

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

(defun myr/activate-theme-tweaks (_theme)
  "Increase the height of the mode line."
  (set-face-attribute 'mode-line nil
		              :box `(:line-width 2 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil
		              :box `(:line-width 2 :color ,(face-attribure 'mode-line-inactive :background)))

  ;; Fixed tab bar faces
  (set-face-attribute 'tab-bar nil :foreground (face-attribute 'mode-line :foreground))
  (set-face-attribute 'tab-bar-tab nil :weight 'bold))

(advice-add 'enable-theme :after #'myr/activate-theme-tweaks)

;; -- Mode Line -----

(use-package doom-modeline
  :ensure nil
  :demand t
  :config (doom-modeline-mode 1))

(setq inhibit-compacting-font-cache t)

;; -- Icons for Dired-mode + Modeline + whole Emacs sesion
(use-package nerd-icons
  :if window-system
  :straight nil
  :demand t)

(use-package all-the-icons
  :if window-system
  :straight nil
  :demand t)
(use-package all-the-icons-dired
  :if window-system
  :straight nil
  :defer 1
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :if window-system
  :straight nil
  :defer 1
  :after all-the-icons)

(use-package all-the-icons-nerd-fonts
  :if window-system
  :straight nil
  :defer 1
  :after all-the-icons)

;; -- Timers -----

(defun myr/tmr-mode-line ()
  "Emacs timer function in modeline."
  (if (not (and (boundp 'tmr--timers)
		        tmr--timers))
      ""
    (propertize (format "  %s: %s"
			            (tmr--format-remaining (car tmr--timers))
			            (tmr--timer-description (car tmr--timers)))
		        'tab-bar '(:foreground "orange"))))

;;; -- Save minibuffer History -----
(use-package savehist
  :straight t
  :config
  (setq history-length 25)
  (savehist-mode 1))

;;; -- Make Help more Helpfull -----

(use-package helpful
  :ensure nil
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

(add-to-list 'auto-mode-alist '("\\.info\\'" . info-on-current-buffer))


(add-hook 'before-save-hook 'time-stamp)

;; -- Backups -----
(setq version-control t
      kept-old-versions 255
      kept-new-versions 1024
      delete-old-versions t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/cache/saves")))

(let ((bak-dir (expand-file-name "~/.emacs.d/cache/saves")))
  (when (and (file-exists-p bak-dir)
             (file-directory-p bak-dir))
    (start-process (concat "delete old backup files in " bak-dir)
                   "*Messages*" "find" bak-dir "-size" "+1M" "-mtime" "+90" "-delete")))

;; -- Emacs -----
(use-package gcmh
  :straight t
  :demand t
  :config
  (gcmh-mode 1))

(use-package ag
  :straight t
  :commands (ag))

(use-package async
  :straight t
  :after dired
  :config
  (dired-async-mode 1))

(use-package centered-cursor-mode
  :disabled t
  :config
  (defun my-info-mode-hook-center-cursor ()
    (centered-cursor-mode))
  (setq Info-mode-hook 'my-info-mode-hook-center-cursor))

(use-package multiple-cursors
  :disabled t
  :ensure nil
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this))

(use-package expand-region
  :ensure nil
  :commands (er/expand-region)
  :custom (shift-select-mode nil)
  :bind (:map shinmera-global-map
              ("C-q" . er/expand-region)))

(use-package beframe
  :ensure nil
  :init
  (beframe-mode))

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

(with-eval-after-load 'consult
  (defface beframe-buffer
	'((t :inherit font-lock-string-face))
	"Face for `consult` framed buffers.")

  (defun myr/beframe-buffer-names-sorted (&optional frame)
	"Return the list of buffers from `beframe-buffer-names` sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
	(beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (consult-customize consult--source-buffer :hidden t :default nil)

  (defvar beframe-consult-source
	`( :name      "Frame-specific buffers (current frame)"
	   :narrow    ?F
	   :category  buffer
	   :history   beframe-history
	   :items     ,#'myr/beframe-buffer-names-sorted
	   :action    ,#'switch-to-buffer
	   :state     ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

;; -- Editing Configuration -----
(setq-default tab-width 4
	          indent-tabs-mode nil
              indicate-empty-lines t
              indicate-buffer-boundaries 'left)

(use-package ws-butler
  :hook ((text-mode prog-mdoe) . ws-butler-mode))

;; Revert buffrs when the undenying file has changed
(global-auto-revert-mode 1)

(use-package paren
  :straight t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363E4A")
  (show-paren-mode 1))

;; -- App Launcher -----
(use-package app-launcher
  :straight t
  :demand t
  :straight '(app-launcher :host github
                           :repo "SebastienWae/app-launcher"))

;; -- Start the Daemon -----
(load "server")
(unless (server-running-p)
  (server-start))
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (local-set-key (kbd "C-x k") 'server-edit)))

(provide 'myr-core)
;;; myr-core.el ends here
