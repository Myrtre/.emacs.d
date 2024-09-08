;;; package --- File Manager -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Dirvish File Manager -----
(use-package dirvish
  :ensure nil
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-acces-entries
   '(("h" "~/"             "Home")
     ("d" "~/downloads/"   "Downloads")
     ("m" "/mnt/"          "Drives")))
  :config
  (dirvish-side-follow-mode)
  (setq dirvish-attributes
        '(vc-state subtree-state collapse file-time file-size))
  (setq dirvish-use-header-line 'global
        dirvish-header-line-height '(25 . 35)
        dirvish-mode-line-height 25)
  (setq dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-preview-dispatchers
        '(image gif video audio pdf archive))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; Mouse Tweak
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dirvish-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dirvish-find-file))

(provide 'myr-dirvish)
;;; myr-dirvish.el ends here
