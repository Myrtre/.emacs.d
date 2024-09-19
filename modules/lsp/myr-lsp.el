;;; myr-lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Load all kind of LSP-modes
(require 'myr-lsp-lisp)
(require 'myr-lsp-web)
(require 'myr-lsp-lua)

(require 'myr-lsp-misc)

(provide 'myr-lsp)
;;; myr-lsp.el ends here
