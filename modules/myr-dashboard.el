;;; myr-dashboard.el --- Dashboard -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

(use-package page-break-lines
  :straight t)
(use-package nerd-icons
  :defer t
  :ensure nil)
(use-package all-the-icons
  :defer t
  :ensure nil)

;; -- Dashboard Banner Title
(defcustom banner-title
  (format "[ᛗ ᛃ ᚱ ᛏ ᚱ ᛖ  %s  ᛖ ᛞ ᛁ ᛏ ᛟ ᚱ]"
          (propertize (nerd-icons-flicon "nf-linux-gnu_guix") 'face '(:height 1.5 :v-adjust -0.35  :weight bold :foreground "#FFCC00")))
  "Myr/Emacs banner title - Myrtre Editor in rune style."
  :type 'string
  :group 'myr/dashboard)

(defcustom myr-messages
  '("\'Be safe, Friend. Don't you dare go to Hollow.\' - Laurentius"
    "\'I am well pleased to see you safe. May the flames guide your way.\' - Anri"
    "\'Seek strenght... the rest will follow.\' - Vendrick"
    "\'Gavlan wheel, Gavlan deal!\' - Gavlan")
  "Own motivational Messages over default ones.  These are quotes from FromSoft Games bc they are Masterpieces."
  :type 'list
  :group 'myr/dashboard)

;; -- Dashboard -----
(use-package dashboard
  :ensure nil
  :bind (:map dashboard-mode-map
              ("n" . 'dashboard-next-line)
              ("p" . 'dashboard-previous-line))
  :after nerd-icons
  :diminish (nerd-icons-install-fonts)
  :config
  (setq dashboard-footer-icon nil
        dashboard-set-footer t
        dashboard-footer-messages myr-messages)

  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(nerd-icons-codicon "nf-cod-octoface" :height 1.5 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/Myrtre/.emacs.d")) nil "" " |")
           (,(nerd-icons-codicon "nf-cod-refresh" :height 1.5 :v-adjust 0.0)
            "Update"
            "Update Zmacs"
            (lambda (&rest _) (auto-package-update-maybe)) warning "" " |")
           (,(nerd-icons-faicon "nf-fa-flag" :height 1.5 :v-adjust 0.0) nil
            "Report a BUG"
            (lambda (&rest _) (browse-url "https://github.com/Myrtre/.emacs.d/issues/new")) error "" ""))
          ;; Empty line
          (("" "\n" "" nil nil "" ""))
          ;; Keybindings
          ;; 30
          ((,(nerd-icons-faicon "nf-fa-search" :height 1.0 :v-adjust -0.1)
            " Find file" nil
            (lambda (&rest _) (find-file)) nil "" "             SPC .  "))
          ((,(nerd-icons-octicon "nf-oct-file_directory" :height 1.0 :v-adjust -0.1)
            " Open project" nil
            (lambda (&rest _) (project-switch-project)) nil "" "          C-x p p"))
          ((,(nerd-icons-octicon "nf-oct-three_bars" :height 1.1 :v-adjust -0.1)
            " File explorer" nil
            (lambda (&rest _) (dirvish)) nil "" "         SPC d d"))
          ((,(nerd-icons-codicon "nf-cod-settings" :height 1.2 :v-adjust -0.1)
            " Open settings" nil
            (lambda (&rest _) (open-config-file)) nil "" "         SPC c c"))))

  (setq dashboard-items '((recents    . 8)
                          (projects   . 4)))
  
  (setq dashboard-banner-logo-title banner-title ;(format "%s" "[ᛗ ᛃ ᚱ ᛏ ᚱ ᛖ  ᛖ ᛞ ᛁ ᛏ ᛟ ᚱ]")
        dashboard-image-banner-max-height 305
        dashboard-startup-banner    "~/.emacs.d/bin/img/emacs_banner.xpm"
        dashboard-set-navigator     t
        dashboard-navigation-cycle  t
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        dashboard-set-heading-icons t
        dashboard-set-file-icons    t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-projects-switch-function 'project-switch-project)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline 
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))
  (dashboard-setup-startup-hook)
  :custom-face
  (dashboard-banner-logo-title ((t (:foreground nil :weight light :family "Noto Sans Mono" :height 1.5))))
  (dashboard-navigator-buttons ((t (:foreground nil :weight bold  :family "Hermit" :slant normal :height 1.0))))
  (dashboard-heading ((t (:foreground nil :weight bold :family "Hermit"))))
  (dashboard-footer ((t (:foreground nil :weight normal :family "Hermit" :slant italic :height 0.9))))
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil))))

(provide 'myr-dashboard)
;;; myr-dashboard.el ends here
