;;; package --- Developer Mode -*- mode: elisp; lexical-binding: t; -*-
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
         web-mode
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

;; -- Project.el -----
;; Native?

;; -- Eglot -----
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-r" . eglot-rename))
  :hook (((c-mode c++-mode) . eglot-ensure)
         ((js2-mode typescript-mode) . eglot-ensure)
         (rust-mode . eglot-ensure)
         (scheme-mode . eglot-ensure)
         (common-lisp-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (charp-mode . eglot-ensure))

  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)

  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

;; -- Code Formating ------
(use-package apheleia
  :ensure nil
  :hook (prog-mode . apheleia-mode))

(use-package lispy
  :ensure nil
  :disabled t
  :hook (emacs-lisp-mode scheme-mode))

(use-package lispyville
  :ensure nil
  :if (and (boundp 'evil) evil-mode)
  :disabled t
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

;; LSP Serivces
(require 'myr-lsp)
(provide 'myr-dev)

;;; myr-dev.el ends here
