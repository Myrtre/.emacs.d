;;; myr-lsp-lisp.el --- Lisp LSP-mode -*- mode: elisp; lexical-bindings: t; -*-
;;; Commentary:
;;; Code:
(require 'myr-straight)

;; -- Utilites -----
(use-package macrostep
  :straight nil
  :commands (macrostep-mode maccrostep-expand))

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

;; -- Emacs Lisp -----
(use-package elisp-mode
  :straight (:type built-in)
  :custom
  (elisp-loop-indent-subclauses nil)
  (lisp-loop-indent-forms-like-keywords t)
  (lisp-lambda-list-keywords-parameter-alignment t))

;; -- Common lisp -----
(use-package common-lisp-mode
  :straight (:type built-in)
  :mode
  ("\\.lisp\\'" . common-lisp-mode)
  ("\\.asd\\'"  . common-lisp-mode)
  ("\\.sexp\\'" . common-lisp-mode))

;; -- Slynk -----
(use-package sly
  :straight nil
  :init
  (progn
    (require 'sly-autoloads)
    (add-hook 'sly-mode-hook
              (lambda ()
                (unless (sly-connected-p)
                  (save-excursion (sly)))))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((common-lisp-mode . eglot-ensure)
                                        (emacs-lisp-mode . eglot-ensure)
                                        (scheme-mode . eglot-ensure))))

(provide 'myr-lsp-lisp)
;;; myr-lsp-lisp.el ends here
