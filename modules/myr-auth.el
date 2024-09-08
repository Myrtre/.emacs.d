;; -*- lexical-binding: t; -*-
(require 'myr-straight)


;; -- Use Emacs for Pinentry -----

(use-package pinentry
  :config
  (unless (or myr/is-term
              (eq system-type 'window-nt))
    (setq epa-pinentry-mode 'loopback)
    (pinentry-start)))

;; -- OAuth2

(use-package oauth2
  :straight t)


(provide 'myr-auth)
