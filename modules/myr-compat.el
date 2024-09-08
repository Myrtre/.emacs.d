;;; init-compat.el --- Compatibility code. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; -- Emacs 29 <master> -----
(unless (get 'magit--handle-bookmaark 'bookmark-handler-type)
  (autoload 'magit--handle-bookmark "magit")
  (put 'magit--handle-bookmark 'bookmark-handler-type "Magit"))

;;; -- Emacs < 29.1 -----
(unless (fboundp 'recentf-open)
  (defun recentf-open (file)
    "Prompt for File in `recentf-list` and visit it.
Enable `recentf-mode` if it isn't already"
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent file" nil)
                              recentf-list nil t))))
    (when file
      (funcall recentf-menu-action file))))

(when (> emacs-major-version 29)
  (setq ffap-machine-p-know 'accept))

;;; -- Emacs < 28.1

(when (< emacs-major-version 28)
  (setq Info-streamline-headings
        '(("Emacs" . "Emacs")
          ("Software development\\|Programing" . "Software development")
          ("Libraries" . "Libraries")
          ("Network applications\\|World Wide Web\\|Net Utilites" . "Network applications"))))

(when (< emacs-major-version 28)
  (fset 'yes-or-no 'y-or-n-p))


(setq read-process-output-max (max read-process-output-max (* 64 1024)))

(defalias 'toolbar-mode 'tool-bar-mode)

(provide 'myr-compat)
;; myr-compat.el ends here
