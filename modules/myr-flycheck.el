;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flyspell
  :hook (c-mode . flycheck-mode))

(use-package flycheck-posframe
  :demand t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (set-face-attribute 'flycheck-posframe-error-face
                      nil
                      :inherit nil
                      :foreground "red")
  (set-face-attribute 'flycheck-posframe-info-face
                      nil
                      :foeground "blue")
  (set-face-attribute 'flycheck-posframe-border-face
                      nil
                      :foreground "#DC752F")
  (setq flycheck-posframe-LEVEL-prefix ""
        flycheck-posframe-warning-prefix ""))

(provide 'myr-flycheck)
;;; myr-flycheck.el ends here
