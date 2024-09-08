;;; package --- System Setting Init -*- mode: elisp; lexical-binding: t; no-byte-compl
;;; Commentary:
;;; Code:

(require 'map) ;; Need for map-merge

(defvar myr/system-settings)
(setq myr/system-settings
      (append
       ;; Put all system specific settings at the fron so that their values
       ;; found first
       (when (equal (system-name) "jotunheim")
	     '(((desktop/dpi . 180)
	        (emacs/default-face-size . 102)
	        (emacs/variable-pitch-face-size . 115)
	        (emacs/fixed-pitch-face-size . 102))))
       ;; Default --
       '((desktop/background . "wallhaven-498e2w")
	     (emacs/default-face-size . 110)
	     (emacs/variable-pitch-face-size . 120)
	     (emacs/fixed-pitch-face-size . 110))))

;;; per-system-settings.el ends here
