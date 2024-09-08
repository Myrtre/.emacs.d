;;; package --- -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Org Mode Extensions -----
(use-package org-modern
  :ensure nil
  :after org
  :hook (org-mode . org-modern-mode))

(use-package toc-org
  :ensure nil
  :after org
  :commands (toc-org-enable)
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Enable Org bullets
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(use-package org-roam
  :ensure nil
  :bind ()
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(provide 'myr-org)
;;; myr-org.el ends here
