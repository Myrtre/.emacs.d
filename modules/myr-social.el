;;; myr-social.el --- Social stuffs -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

(use-package emojify
  :ensure nil
  :config (emojify-mode))

;; -- Mastodon Instance -----
(use-package mastodon
  :ensure nil
  :config
  (setq mastodon-instance-url "https://mastodon.social"
        mastodon-active-user "myrtre")
  (mastodon-discover))

;; -- Matrix -----
;; TODO: Matrix instance element?


(provide 'myr-social)
;;; myr-social.el ends here
