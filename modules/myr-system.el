;;; myr-system.el --- System (GUIX) configuration -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Systemvars + User -----
(setq user-full-namr        "Davy Gut"
      user-login-name       "Myrtré"
      user-real-login-name  "Gút Dávid"
      user-mail-address     "myrtre@proton.me")

(setq gc-cons-threshold (* 50 1000 1000))

;; -- Guix Packages -----
(use-package guix
  :ensure nil)
(rune/leader-key
  "s"   '(:ignore t :wk "System+ <prefix>")
  "s i" '(guix-installed-user-packages :wk "Installed user packages")
  "s I" '(guix-installed-user-packages :wk "Installed system packages")
  "s p" '(guix-installed-user-packages :wk "Packages by name")
  "s P" '(guix-installed-user-packages :wk "Guix pull")
  "s g" '(guix :wk "Guix"))

(use-package daemons
  :ensure nil)


;; -- Pulse-audio -----
(use-package pulseaudio-control
  :ensure nil)

(use-package proced
  :straight t
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))


(provide 'myr-system)
;;; myr-system.el ends here
