;;; functions.el --- Custom functions
;;; Commentary:
;; This file contains some utility Emacs enhanced functions.
;;; code:

(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'my/move-beginning-of-line)

(defun my/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (my/indent-buffer)
        (message "Indented buffer.")))
    (whitespace-cleanup)))

(global-set-key (kbd "C-c i") 'my/indent-region-or-buffer)

(defun my/smart-open-line ()
  "Insert an empty line after he current line.
Position the cursor at it's beginning."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursors at it's beginning."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") 'my/smart-open-line)
(global-set-key (kbd "M-o") 'my/smart-open-line-above)

(defun my/comment-or-uncomment-region ()
  "Comment or uncomment a region.
Comment the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-c c") 'my/comment-or-uncomment-region)

(defun my/kill-ring-save ()
  "Copy the region selected or the current line if no region is active."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (kill-ring-save beg end)))

(global-set-key (kbd "M-w") 'my/kill-ring-save)

(provide 'functions)

;;; functions.el ends here
