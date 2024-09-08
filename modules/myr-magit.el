;;; package --- Magit -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Magit -----
(use-package magit
  :ensure nil
  :commands magit
  :custom
  (git-commit-fill-column 9999)
  (git-commit-summary-max-length 9999)
  (git-commit-finish-query-functions nil)
  (magit-delete-by-moving-to-trash nil)
  (magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package magit-todos
  :ensure nil
  :demand t
  :after magit
  :custom
  (magit-todos-keywords-list (mapcar #'car hl-todo-keyword-faces))
  (magit-todos-auto-group-items 50)
  (magit-todos-exclude-globs '(".git/" "docs/"))
  :config
  (magit-todos-mode))

;; -- Git -----
(use-package git-timemachine
  :ensure nil
  :commands (git-timemachine))

(use-package git-modes
  :ensure nil)


(provide 'myr-magit)
;;; myr-magit.el ends here
