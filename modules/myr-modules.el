;; -*- mode: elisp; lexical-binding: t; -*-
(require 'cl-lib)

;; -- Sep-Modules -----
(defvar core-modules
  '(:portability :settings :core :keys :dirvish :shell :system))
(defvar ui-modules
  '(:interface :dashboard))
(defvar misc-modules
  '(:org :spell :social))
(defvar developer-modules
  '(:magit :dev)) ;;  ~ :spell.el

;; TODOS: :mail , :spell, 
(defvar myr/module-list
  (cons* core-modules
         ui-modules
         misc-modules))
(defvar myr/default-module-list
  (cons* core-modules
         ui-modules
         misc-modules
         developer-modules))

;; -- Load Modules -----
(defcustom myr/modules (copy-sequence myr/default-module-list)
  "Which modules to load on startup"
  :type `(set ,@(cl-loop for module in myr/module-list
                         collect `(const ,module)))
  :group 'myr)

(dolist (module myr/modules)
  (let ((name (intern (concat "myr-" (substring (symbol-name module) 1)))))
    (require name)))

(provide 'myr-modules)
