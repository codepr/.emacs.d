
;;; init.el --- Codep's configuration entry point.
;;; Commentary:
;; This file contain my personal Emacs configuration
;;; code:
(setq user-full-name "Andrea Giacomo Baldan"
      user-mail-address "a.g.baldan@gmail.com")

;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Default to visual-line-mode
(setq global-visual-line-mode 1)

;; Get the emacs directory (same directory as this file).
(defvar emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Code directory.")

;; configuration directory, where configuration lisp are located
(defvar lisp-dir (concat emacs-dir "/lisp/"))

;; This function will load an elisp file in this directory.
(defun load-lisp-init-file (filename)
  "Load the .el file represented by FILENAME."
  (load (expand-file-name filename lisp-dir)))

;; This function checks if a filename is an elisp file.
(defun is-lisp-file-p (name)
  "Check if NAME is a .el filename."
  (and (> (length name) 2) (string= ".el" (substring name -3))))

;; Add any filenames you don't want loaded here! (as "strings")
(setq disabled-module-list '())

;; Load all org-mode init files in this directory
(dolist (file (directory-files-and-attributes lisp-dir))
  (when (is-lisp-file-p (car file))
    (unless (member (car file) disabled-module-list)
      (load-lisp-init-file (car file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN AUTO-GENERATED SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :weight Normal :family "DejaVu Sans Mono"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :slant italic)))))
