;;; myr-lsp-Lua.el --- Lua development modules -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'myr-straight)

(use-package lua-mode
  :straight nil
  :commands (lua-mode)
  :mode
  ("\\.lua\\'" . lua-mode))


(use-package fennel-mode
  :straight nil
  :commands (fennel-mode)
  :mode
  ("\\.fnl\\'" . fennel-mode))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((fennel-mode . ("fennel-ls")))))

(provide 'myr-lsp-lua)
;;; myr-lsp-lua.el ends here
