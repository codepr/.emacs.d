
;;; init.el --- Codep's configuration entry point.
;;; Commentary:
;; This file contain my personal Emacs configuration
;;; code:
(setq user-full-name "Andrea Giacomo Baldan"
      user-mail-address "a.g.baldan@gmail.com")
;; Need package for package management.
(require 'package)
;; Need cl because some packages load with errors otherwise
(eval-when-compile (require 'cl))
;; Get the emacs directory (same directory as this file).
(defvar emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Code directory.")
;; configuration directory, where configuration orgs are located
(defvar conf-dir (concat emacs-dir "/conf/"))
;; add to load-path conf folder for personal configurations
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/conf/"))
;; elpa packages archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
;; This function will install a package if not already installed.
(defun require-package (package-list)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))
;; Fetch the list of packages available (only if the package list isn't built)
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Need org mode for babel
(require 'org)
;; This function will load an org mode file in this directory.
(defun load-org-init-file (filename)
  (org-babel-load-file (expand-file-name filename conf-dir)))

;; This function checks if a filename is an org mode file.
(defun is-org-file-p (name)
  (and (> (length name) 3) (string= ".org" (substring name -4))))

;; Add any filenames you don't want loaded here! (as "strings")
(setq disabled-module-list '())
;; Load all org-mode init files in this directory!!
(dolist (file (directory-files-and-attributes conf-dir))
  (when (is-org-file-p (car file))
    (unless (member (car file) disabled-module-list)
      (load-org-init-file (car file)))))

;;(byte-recompile-directory ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN AUTO-GENERATED SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'org
  '(progn
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
      '(default ((t (:height 100 :weight Normal :family "Monaco")))))
     (put 'downcase-region 'disabled nil)
     (put 'upcase-region 'disabled nil)
))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit anaconda-mode auctex ido-vertical-mode idomenu smex ido-ubiquitous helm-ag helm-projectile projectile helm flycheck ensime neotree php-mode base16-theme greymatters-theme solarized-theme expand-region which-key markdown-mode autopair company-emacs-eclim eclim org-page))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :weight Normal :family "Monaco")))))
(setq line-spacing 3)
