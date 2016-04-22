(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory (expand-file-name "~/org"))
(setq org-default-notes-file(concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-capture)
(setq org-capture-templates
  (quote (("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %? :NOTE:\n%U\n%a\n")
          ("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n %i\n %U \n %a\n")
          ("s" "Snippet" entry (file+headline "~/org/snippets.org" "Snippets")
           "** %?\n %U\n %a\n"))))
(setq org-reverse-note-order t)
(setq org-refile-allow-create-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets (quote ((nil :maxlevel . 6)
                                 (org-agenda-files :maxlevel . 6))))
(setq org-agenda-files
  (delq nil
    (mapcar (lambda (x) (and (file-exists-p x) x))
            '("~/org/notes.org"
              "~/org/snippets.org"))))
