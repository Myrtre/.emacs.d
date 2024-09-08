;;; -- Set up package.el and use-package -----

(require 'package)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;(unless package-archive-contents
;  (package-refresh-contents))

;; -- Package Providers -----
;; Move here from upper version bc don't need to load every time
(require 'server)
(require 'cl-lib)
;(require 'cl)

(defvar *package-lists-fetched* nil)
(defun soft-fetch-package-lists ()
  (unless *package-lists-fetched*
    (package-refresg-contents)
    (setf *package-lists-fetcehd* t)))

(defun ensure-installed (&rest packages)
  (unless (cl-loop for package in packages
                   always (package-locally-installed-p package))
    (message "Trying to install: %s" packages)
    (soft-fetch-package-lists)
    (dolist (package packages)
      (unless (package-locally-installed-p package)
        (package-install package)))))
                                           

;; -- Use-Package -----
(unless (package-installed-o 'use-package)
  (package-install 'use-package))
(require 'use-package)



;; Automatically install packages when not in guix
(setq use-package-always-ensure (not myr/is-guix-system))


(provide 'myr-package.backup)
