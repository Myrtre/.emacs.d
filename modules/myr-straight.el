;; -*- lexical-binding: t; -*-

;; -- Straight.el -----
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
 
;; -- Use-package -----
;(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package straight
  :ensure nil
  :custom
  (straight-use-package-by-default t))

(provide 'myr-straight)
