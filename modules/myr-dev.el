;;; myr-dev.el --- Developer Mode -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Paren Matching -----
(use-package rainbow-delimiters
  :ensure nil
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure nil
  :hook (org-mode
         emacs-lisp-mode
         common-lisp-mode
         scheme-mode
         lua-mode
         fennel-mode
         web-mode
         css-mode
         typescript-mode
         js2-mode))

;; -- Buffer Environments -----
(use-package buffer-env
  :ensure nil
  :custom
  (buffer-env-script-name "manifest.scm"))

;; -- M-x compile -----
(use-package compile
  :straight t
  :custom
  (compilation-scroll-output t))

(setq compilation-environment '("TERM=xterm-256color"))

(defun myr/advice-compilation-filter (f proc string)
  "Compilation filter by (F PROC STRING)."
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'myr/advice-compilation-filter)

(defun rune/auto-recompile-buffer ()
  "Recompile buffer."
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;; -- Flycheck
(use-package flycheck-posframe
  :ensure nil
  :demand t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (set-face-attribute 'flycheck-posframe-error-face
                      nil
                      :inherit nil
                      :foreground "red")
  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foreground "blue")
  (set-face-attribute 'flycheck-posframe-border-face
                      nil
                      :foreground "#DC752F")
  (setq flycheck-posframe-LEVEL-prefix ""
        flycheck-posframe-warning-prefix ""))

;; -- Eglot -----
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-r" . eglot-rename))
  :hook (((c-mode c++-mode) . eglot-ensure)
         ((js2-mode typescript-mode) . eglot-ensure)
         (rust-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (charp-mode . eglot-ensure))

  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil))

;; -- Code Formating ------
(use-package apheleia
  :ensure nil
  :hook (prog-mode . apheleia-mode))

;; LSP Serivces
(add-to-list 'load-path "~/.emacs.d/modules/lsp/")
(require 'myr-lsp)
(provide 'myr-dev)

;;; myr-dev.el ends here
