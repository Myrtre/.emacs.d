;;; early-init.el --- * -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(defvar myr/backup-gc-cons-threshold gc-cons-threshold)
(defun myr/lower-gc-cons-threshold ()
  "Revert back to something slighty bigger than the default."
  (setq gc-cons-threshold (+ myr/backup-gc-cons-threshold 200000))
  (remove-hook 'focus-out-hook #'myr/lower-gc-cons-threshold))

(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 3 nil #'myr/lower-gc-cons-threshold)
            (add-hook 'focus-out-hook  #'myr/lower-gc-cons-threshold)))

(setq gc-cons-threshold (* 1024 gc-cons-threshold))
;;; early-init.el ends here
