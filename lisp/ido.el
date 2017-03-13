;;; package -- Summary
;;; ido.el - Ido configuration
;;; Commentary:
;; AUTHOR:  Andrea Baldan <a.g.baldan@gmail.com>
;; LICENCE: GPL2
;;; Code:

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(provide 'ido)

;;; ido.el ends here
