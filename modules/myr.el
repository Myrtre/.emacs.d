;;;; -*- mode: elisp; lexical-binding: t; -*-


;;; --- System identification -----

(defvar myr/current-distro (or (and (eq system-type 'gnu/linux)
				                    (file-exists-p "/etc/os-release")
				                    (with-temp-buffer
				                      (insert-file-contents "/etc/os-release")
				                      (search-forward-regexp "^ID=\"?\\(.*\\)\"?$")
				                      (intern (or (match-string 1)
						                          "unknown"))))
			                   'unknown))

(defvar myr/is-guix (eql myr/current-distro 'guix))
(defvar myr/is-term (null window-system))

;; Load Pertinent Modules
(require 'myr-functions)
(require 'myr-modules)   ;;|-> load-module wrapper


(when (string= system-name "jotunheim")
  (setq myr/mail-enabled t))


(add-hook 'after-init-hook
          #'(lambda () (rune/toggle-transparency)))
(rune/toggle-transparency)

(provide 'myr)
