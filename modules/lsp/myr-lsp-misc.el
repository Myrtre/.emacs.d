;;; myr-lsp-misc.el --- Misc LSP -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'myr-straight)

(use-package nix-mode
  :straight nil
  :commands (nix-mode)
  :mode
  ("\\.nix\\'" . nix-mode))


(provide 'myr-lsp-misc)
;;; myr-lsp.el ends here
