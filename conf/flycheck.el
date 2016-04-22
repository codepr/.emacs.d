
;;; flycheck.org --- Codep's configuration entry point.
;;; Commentary:
;; This file contain my personal Emacs configuration
;;; code:
(require-package '(flycheck))
(require 'flycheck)
;; flycheck tool
(setq flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
;;; flycheck.org ends here
