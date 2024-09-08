;;; init.el --- Emacs configuration -*- mode: elisp; lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(setq message-log-max (* 10 message-log-max))

;; Load Myr config...

(add-to-list 'load-path "~/.emacs.d/modules/")
(require 'myr)

;; Set the workin' dir to home
(cd "~/")

;; *scratch*
(add-hook 'after-init-hook
          (lambda ()
            (insert (concat ";; " (substring (emacs-version) 0 14)))
            (when (not noninteractive)
              (insert (format " loaded in %s\n" (emacs-init-time))))
            (newline-and-indent) (newline-and-indent)))

;; Enable some features
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'help-fns-edit-variable 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;;; init.el ends here
