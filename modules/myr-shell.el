;;; package -- Shell -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Shell Functions -----
(defun read-file (file-path)
  "Read `FILE-PATH` to buffer."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun rune/get-current-package-version ()
  "Get package version."
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun myr/map-line-to-status-char (line)
  "Map status from LINE."
  (cond ((string-match "^?\\? " line) "?")))

(defun myr/get-git-status-prompt ()
  "Get git status."
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'myr/map-line-to-status-char status-lines)))))

(defun myr/get-prompt-path ()
  "Get prompt path."
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by spaceship-prompt
(defun myr/eshell-prompt ()
  "EShell prompt."
  (let ((current-branch (magit-get-current-branch))
        (package-version (rune/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62AEED"))
     (propertize " Ω " 'face `(:foreground "white"))
     (propertize (myr/get-prompt-path) 'face `(:foreground "#82CFD3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat "> " current-branch) 'face `(:foreground "#C475F0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#E8A206"))))
     (propertize " . " 'face `(:foreground "white"))
     (propertize (forme-time-string "%I:%M:%S %p") 'face `(:foreground "#5A5B7F"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#AECE4A")))
     (propertize " " 'face `(:foreground "white")))))

(use-package xterm-color
  :ensure nil)

(defun myr/eshell-configure ()
  "Configure EShell."
  (require 'magit)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (selq 'eshell-handle-ansi-color eshell-output-filter-functions)

  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function           'myr/eshell-prompt
        eshell-prompt-regexp             "^λ"
        eshell-history-size              10000
        eshell-buffer-maximum-lines      10000
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions     nil))

;; -- Shell Packages -----
(use-package eshell
  :ensure nil
  :config
  (add-hook 'eshell-first-time-mode-hook #'myr/eshell-configure)
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

(use-package exec-path-from-shell
  :ensure nil
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun rune/switch-to-eshell ()
  "Switch to EShell."
  (interactive)
  (if (project-current)
      (call-interactively #'project-eshell)
    (call-interactively #'eshell)))

(global-set-key (kbd "C-c e") #'rune/switch-to-eshell)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "btop" "zsh" "fish" "vim")))

(use-package eshell-syntax-highlighting
  :ensure nil
  :after eshell
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(defun myr/esh-autosuggest-setup ()
  "EShell autosuggest."
  (require 'company)
  (set-face-foreground 'company-preview-common "#4B5668")
  (set-face-background 'company-preview nil))

(use-package esh-autosuggest
  :ensure nil
  :hook (eshell . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5))

(use-package eat
  :ensure nil
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation nil)
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '()))

(use-package vterm
  :ensure nil
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (when (featurep 'evil)
    (advice-add 'evil-collection-vterm-install :before #'vterm-reset-cursor-point)))


(provide 'myr-shell)
;;; myr-shell.el ends here
