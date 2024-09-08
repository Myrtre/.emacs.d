;;; portability.el --- Portability -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Windows Portability
(when (eq system-type 'window-nt)
  (when load-file-name
    (setenv "HOME" (file-name-directory load-file-name))))

(provide 'myr-portability)
;;; myr-portability.el ends here
