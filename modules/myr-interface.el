;;; myr-interface.el --- Interface -*- mode: elisp; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Hydra -----
(use-package hydra
  :ensure nil)

;; -- Completion -----
(use-package corfu
  :ensure nil
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-popupinfo-hide nil)
  (corfu-popupinfo-delay '(3.0 . 2.0))
  :config
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))


(use-package kind-icon
  :ensure nil
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure nil
  :after corfu
  :config
  (add-to-list 'completion-at-point-function #'cape-file)
  (add-to-list 'completion-at-point-function #'cape-dabbrev)
  (add-to-list 'completion-at-point-function #'cape-keyword)
  (addvice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (addvice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


;; -- Orderless -----
(use-package orderless
  :ensure nil
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-style '(orderless-initialism
                                orderless-literal
                                orderless-regexp)))
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  (setq completion-category-overrides
        '((file (styles partial-completion)))))

;; -- Vertico -----
;; + Consult
;; + Marginalia
;; + Savehint
;; + Embark

(use-package icomplete
  :disabled
  :straight t
  :custom
  (icomplete-scroll t)
  (icomplete-show-matches-on-no-input t)
  (icomplete-vertical-prospects-height 1)
  (icomplete-compute-delay 0.05)
  :init
  (icomplete-vertical-mode 1))

(use-package vertico
  :ensure nil
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package vertico-posframe
  :ensure nil
  :after vertico
  :init
  (vertico-posframe-mode 1))

(use-package wgrep
  :ensure nil
  :after consult
  :hook (grep-mode . wgrep-setup))

(use-package consult
  :ensure nil
  :demand t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function (lambda (&rest args)
                                   (apply (if vertico-mode
                                              #'consult-completion-in-region
                                            #'completion--in-region)
                                          args)))
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package marginalia
  :ensure nil
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package embark
  :ensure nil
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("C-d" . embark-act)
         :map embark-region-map
         ("D" . denote-region))
  :config
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  
  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; -- Theme --
(use-package xterm-color
  :ensure nil)


(use-package doom-themes
  :ensure nil
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme
   (pcase (system-name)
     ("jotunheim" 'doom-gruvbox)
     (_ 'doom-monokai-pro))
   t)
  (add-hook 'after-init-hook
            (lambda ()
              (load-theme
               (pcase (system-name)
                 ("jotunheim" 'doom-gruvbox)
                 (_ 'doom-monokai-pro))
               t)))
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(provide 'myr-interface)
;;; myr-interface.el ends here
