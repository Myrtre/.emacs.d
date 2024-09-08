;; -*- lexical-binding: t; -*-
(require 'myr-straight)

;; -- rTorrent Emacs Session -----
(use-package mentor
  :ensure nil
  :config
  (add-to-list 'load-path (expand-file-name "~/wip/mentor"))
  (add-to-list 'load-path (expand-file-name "~/wip/xml-rpc-el"))
  (setq mentor-rtorrent-external-rpc "http://localhost/our-RPC2"))

(provide 'myr-mentor)
