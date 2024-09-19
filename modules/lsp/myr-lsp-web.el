;;; myr-lsp-wev.el --- Web Mode stuffs -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :straight nil
  :commands (web-mode)
  :mode "(\\.\\(html?\\|htm?\\|xhtml?\\|ctml?\\|ejs\\|tsx\\|jsx\\)\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2))

(use-package typescript-mode
  :straight nil
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :straight nil
  :mode "\\.jsx?\\'"
  :config
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil)
  (setq-default js-indent-level 2))

(defun myr/setup-markdown-mode ()
  "Set up frame for Markdown."
  (visual-fill-column-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :straight nil
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'myr/setup-markdown-mode)
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(use-package impatient-mode
  :straight t)
(use-package skewer-mode
  :straight nil)

(use-package yaml-mode
  :straight nil
  :mode "\\.ya?ml\\'")

(use-package lass
  :commands (lass-mode)
  :straight (:type git :repo "https://github.com/Shinmera/LASS")
  :mode ("\\.lass\\'" . lass-mode))


(with-eval-after-load 'simple-httpd
  (add-to-list 'httpd-mime-types '("wasm" . "application/wasm")))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((js2-mode typescript) . ("typescript-language-server" "--stdio"))))

(provide 'myr-lsp-web)
;;; myr-lsp-web.el ends here
