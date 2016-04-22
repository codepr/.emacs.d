;;; autocomplete.el --- Codep's custom ac configuration.
;;; Commentary:
;; Author: Andrea Giacomo Baldan
;; Created: 2016/03/01
;; Last Modified: 2016/03/21
;;; code:
;;(require-package '(auto-complete))
;;(require 'auto-complete)
;;(require 'auto-complete-config)
(require-package '(company))
;; ac
(add-hook 'after-init-hook 'global-company-mode)
;; ac behaviour
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))
(setq company-auto-complete t)
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
;; (ac-config-default)
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)
;; (setq ac-auto-start 3
;;       ac-fuzzy-enable t)
;; (global-auto-complete-mode t)
;; (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))
;;; autocomplete.el ends here
