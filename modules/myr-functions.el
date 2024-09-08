;; -*- mode: elisp; lexical-binding: t; -*-

;; Function to run stuff with Sudo privilige!
(defun rune/sudo ()
  "Run given args with [SUDO] privilige."
  (interactive)
  (let ((position (point)))
    (find-alternate-file (concat "/sudo::"
				                 (buffer-file-name (current-buffer))))
    (goto-char position)))

;; Use thefollowing snippet after you've set the alpha to assing a toggle bind:
(defun rune/toggle-transparency ()
  "Crave for transparency!"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		            ((numberp (cdr alpha)) (cdr alpha))
		            ;; Also handle undocumented (<active> <inactive>) form
		            ((numberp (cadr alpha)) (cadr alpha)))
	          100)
	     '(85 . 50) '(100 . 100)))))

(defun rune/app-launcher ()
  "App-launcher starter"
  (interactive)
  (let ((frame (make-frame '((name . "emacs-run-launcher")
                             (minibuffer . only)
                             (fullscreen . 0)
                             (undecorated . t)
                             (internal-border-width . 10)
                             (width . 80)
                             (height . 11)))))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame frame))))



(provide 'myr-functions)
