;;; package --- Setting Loader* -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; -- Variables -----
(defvar myr/default-font "Hermit"
  "The font used for `default` and `fixed-pitch` faces.")
(defvar myr/varable-pitch-font "Hermit"
  "The font used for `default` and `fixed-pitch` faces.")

(defvar myr/fixed-pitch-font "Iosevka Aile"
  "The font used for `default` and `fixed-pitch` faces.")


;; -- Emacs configurations -----
(defun myr/load-system-settings ()
  "Load default settings by hostname."
  (interactive)
  (load-file "~/.dotfiles/.emacs.d/per-system-settings.el"))

(defun myr/reload-configuration ()
  "Reload emacs-configuration file..."
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(defun myr/system-settings-get (setting)
  (alist-get setting myr/system-settings))

;; Load settings for the first time
(myr/load-system-settings)


(provide 'myr-settings)
