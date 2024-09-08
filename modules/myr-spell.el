;; -*- lexical-binding: t; -*-
(require 'myr-straight)

;; -- Spelling -----
(setq used-spelling-dictionaries '("en_US" "en_GB" "hu_HU"))

(use-package jinx
  :defer 2
  :straight nil  ;; Already installed via Guix
  :custom (jinx-languages (car used-spelling-dictionaries))
  :hook
  (emacs-startup . global-jinx-mode))

(use-package ispell
  :demand t
  :straight nil  ;; Already installed via Guix
  :custom (ispell-dictionary (car used-spelling-dictionaries)))

(defun rune/switch-dictionary ()
  (interactive)
  (let* ((old ispell-current-dictionary)
         (new (nth (mod (1+ (cl-position old used-spelling-dictionaries))
                        (length used-spelling-dictionaries))
                   used-spelling-dictionaries)))
    (ispell-change-dictionary new)
    (message "Dictionary switched from %s to %s" old new)))


(provide 'myr-spell)
