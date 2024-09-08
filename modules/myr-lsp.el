;;; package --- LSP wrapper -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- LSP Services -----

;;; -- Emacs Lisp -----
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;; -- Common Lisp -----
(use-package sly
  :disabled
  :mode "\\.lisp\\'")

;;; -- Scheme -----
;; There is no package so add as hook
;;(use-package scheme-mode
;;  :ensure nil
;;  :mode "\\.sld\\'")
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile
        geiser-active-implementations '(guile)
        geiser-implementations-alist '(((regexp "\\.scm$") guile))))
(use-package geiser-guile
  :after geiser
  :ensure t)

;;; -- Rust -----
(use-package rustic
  :commands (rustic-mode)
  :mode
  ("\\.rs\\'" . rustic-mode))

(use-package lsp-mode
  :demand t
  :after (rustic))


;;; -- Lua -----
(use-package lua-mode
  :commands (lusa-mode)
  :mode
  ("\\.lua\\'" . lua-mode))

(use-package company-lua
  :demand t
  :after (company lua-mode)
  :config
  (define-hook lua-mode-hook ()
               (setq-local company-backends '((company-lua
                                               company-etags
                                               company-dabbrev-code)))))
(use-package flymake-lua
  :demand t
  :after (lua-mode))

;;; -- JavaScript/Typescript -----
(use-package js2-mode
  :commands (js2-mode js2-minor-mode)
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . js2-minor-mode)
  (js2-mode . (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind (:map js-mode-map ("M-." . nil))
  :config (font-lock-add-keywords 'js2-mode '(("self" . font-lock-constant-face))))

(use-package xref-js2
  :demand t
  :after (js2-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2))

;;; -- Java -----
(use-package jdee
  :commands (jdee-mode)
  :mode ("\\.java\\'" . jdee-mode))

;; TODOS
;; + Kotlin
;; + Clojure

;;; -- Go -----
(use-package go-mode
  :commands (go-mode)
  :mode ("\\.go\\'" . go-mode))

(use-package company-go
  :demand t
  :after (go-mode company))


;;; -- GLSL [OpenGL Shading Languange] -----
(use-package glsl-mode
  :commands (glsl-mode)
  :mode
  ("\\.glsl\\'" . glsl-mode))

(use-package company-glsl
  :demand t
  :after (company glsl-mode))

(use-package hlsl-mode
  :commands (hlsl-mode)
  :straight (:type git :repo "https://github.com/darfink/hlsl-mode")
  :mode
  ("\\.hlsl\\'" . hlsl-mode)
  ("\\.fx\\'" . hlsl-mode))

;;; -- Misc -----
;; Other LSP stuff

(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :mode ("\\'Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :commands (yaml-mode)
  :mode ("\\.yml\\'" . yaml-mode))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :hook
  (markdown-mode . olivetti-mode)
  (gfm-mode . olivetti-mode))

(use-package markless
  :commands (markless-mode)
  :mode
  ("\\.mess\\'" . markless-mode)
  ("\\.markless\\'" . markless-mode)
  :hook (markless-mode . olivetti-mode))

(use-package csv-mode
  :commands (csv-mode)
  :mode ("\\.csv\\'" . csv-mode))

(use-package git-modes
  :commands (gitattributes-mode gitconfig-mode gitignore-mode)
  :mode
  ("\\.gitattributes\\'" . gitattributes-mode)
  ("\\.gitconfig" . gitconfig-mode)
  ("\\.gitmodules" . gitconfig-mode)
  ("\\.gitignore" . gitignore-mode))

(use-package nginx-mode
  :commands nginx-mode)

;;; -- Web LSP Snippets -----

(use-package web-mode
  :commands (web-mode)
  :mode
  ("\\.htm\\'" . web-mode)
  ("\\.html\\'" . web-mode)
  ("\\.xhtml\\'" . web-mode)
  ("\\.ctml\\'" . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2))

(use-package lass
  :commands (lass-mode)
  :straight (:type git :repo "https://github.com/shinmera/LASS")
  :mode ("\\.lass\\'" . lass-mode))



;; -- Snippets -----
;; TODO: move eglot to here from dev!

(provide 'myr-lsp)
;;; myr-lsp.el ends here
