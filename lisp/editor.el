;;; editor.el --- Codep's custom editing configuration.
;;; Commentary:
;; Author: Andrea Giacomo Baldan
;; Created: 2016/03/01
;; Last Modified: 2016/03/21
;;; code:
;; general utilities packages

;; start maximized
(toggle-frame-maximized)
(blink-cursor-mode -1)
;; icomplete minibuffer
(icomplete-mode t)
;; encoding UTF-8
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Removes *Completions* and *scratch* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))
;; remove *messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; inhibit startup message
(setq inhibit-startup-message t)
;; default cursor gery
;; (setq default-frame-alist '((cursor-color . "grey")))
;; convert any change made on file to the current buffer
(global-auto-revert-mode)
;; no backup
(setq make-backup-files nil)
;; delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; window settings
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
;; global font lock maximum level
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
;; scroll till bottom / top
(setq scroll-error-top-bottom t)
;; uniquify buffer filenames
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")
;; show keystroke in progress
(setq echo-keystrokes 0.1)
;; show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)
;; don't break lines
(setq-default truncate-lines t)
;; better word-wrapping
(visual-line-mode 1)
;; highlight matching parenthesis
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
;; auto fill
(auto-fill-mode 1)
;; preservative scroll, C-v and M-v returns cursor to the same position
(setq scroll-preserve-screen-position 'always)
;; 80 chars len lines
(setq-default fill-column 80)
;; column number mode
(setq column-number-mode t)
;; Line numbering on code buffers
(add-hook 'prog-mode-hook (lambda () (linum-mode t)))
;; highlight current line
(global-hl-line-mode 1)
;; sentences ending with one space
(setq sentence-end-double-space nil)
;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; reduce frequency of garvage collector
(setq gc-cons-threshold 50000000)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
;; ibuffer sort by major mode
(setq ibuffer-default-sorting-mode 'major-mode)
;; RET autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)
;; fix scroll, make it smooth
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000)
;; indent style
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode nil)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'editor)

;;; editor.el ends here