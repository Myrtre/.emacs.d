;;; myr-keys.el --- Keybinds -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)


;; -- Evil mode -----
(use-package evil
  :ensure nil
  :custom
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :ensure nil
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

;; -- General Keybinds -----
(use-package general
  :ensure nil
  :config
  (general-evil-setup t))
;; Define Key
(general-create-definer rune/leader-key
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "M-SPC")

;; Uncategorized
(rune/leader-key
  nil)

;; File Operations
(rune/leader-key
  "."   '(find-file :wk "Find file")
  "f"   '(:ignore t :wk "File+ <prefix>")
  "f f" '(find-file :wk "Find file")
  "f r" '(recentf-open :wk "Recently opened files")
  "f C" '(copy-file :wk "Copy file")
  "f D" '(delete-file :wk "Delete file")
  "f R" '(rename-file :wk "Rename file")
  "f S" '(write-file :wk "Save file as..."))

;; Helpers
(rune/leader-key
  "h"     '(:ignore t :wk "Helpers+ <prefix>")
  "h f"   '(describe-function :wk "Describe function")
  "h v"   '(describe-variable :wk "Describe variable")
  "h r r" '(myr/reload-configuration :wk "Describe function"))

;; Buffer
(rune/leader-key
  "b"  '(:ignore t :wk "Buffer+ <prefix>")
  "b b" '(switch-to-buffer :wk "Switch buffer")
  "b k" '(kill-this-buffer :wk "Kill this buffer")
  "b n" '(next-buffer :wk "Next buffer")
  "b p" '(previous-buffer :wk "Previous buffer")
  "b r" '(revert--buffer :wk "Reload buffer"))

;; Magit
(rune/leader-key
  "m" '(magit-status :wk "Magit status"))

;; Cursor
(rune/leader-key
  "C-*"     '(:ignore t :wk "Cursor+ <prefix>")
  "C-f"   '(mc/mark-next-like-this :wk "Select next line")
  "C-b"   '(mc/mark-previous-like-this :wk "Select previous line")
  "C-a"   '(mc/mark-all-like-this :wk "Select All line")
  "C-S-f" '(mc/unmark-next-like-this :wk "Deselect next line")
  "C-S-b" '(mc/unmark-previous-like-this :wk "Deselect previous line"))

;; Window/Frame
(rune/leader-key
  "w"   '(:ignore t :wk "Window+ <prefix>")
  "w c" '(evil-window-delete :wk "Close this window/frame")
  "w n" '(evil-window-vnew :wk "New window/frame")
  ;; Splits
  "w s" '(evil-window-split :wk "Split horizontal")
  "w S" '(evil-window-vsplit :wk "Split vertical")
  ;; Motions
  "w h" '(evil-window-left :wk "Focus left")
  "w j" '(evil-window-down :wk "Focus down")
  "w k" '(evil-window-up :wk "Focus up")
  "w l" '(evil-window-right :wk "Focus right"))

;; Toggles
(rune/leader-key
  "t"   '(:ignore t :wk "Toggle+ <prefix>")
  "t l" '(display-line-numbers-mode :wk "Toggle line-numbers")
  "t m" '(menu-bar-mode :wk "Toggle menu-bar")
  "t t" '(tool-bar-mode :wk "Toggle tool-bar")
  "t T" '(rune/toggle-transparency :wk "Toggle transparency"))

;; FM - Dirvish
(rune/leader-key
  "d"   '(:ignore t :wk "Dirvish+ <prefix>")
  "d ." '(dirvish-fd :wk "[D] Find file")
  "d s" '(dirvish-side :wk "[D] Open side")
  "d y" '(dirvish-yank-menu :wk "[D] Yank Menu")
  "d a" '(dirvish-quick-access :wk "[D] Quick Access")
  "d d" '(dirvish :wk "[D] Divish ~" ))

;; Zoom
(rune/leader-key
  "z"   '(:ignore t :wk "Zoom+ <prefix>")
  "z i" '(text-scale-increase :wk "Zoom in")
  "z o" '(text-scale-decrease :wk "Zoom out"))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


;; -- Which-key -----
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-popup-type 'side-window
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-min-display-lines 5
        which-key-max-display-columns nil
        which-key-side-window-max-height 0.25
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.6
        which-key-max-description-length 25
        which-key-show-remaining-keys nil
        which-key-separator " > "))

(provide 'myr-keys)
;;; myr-keys.el ends here
