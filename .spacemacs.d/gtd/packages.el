;;; packages.el --- gtd layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: jj <jj@jj-vivobooks15x530un>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `gtd-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `gtd/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `gtd/pre-init-PACKAGE' and/or
;;   `gtd/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst gtd-packages
  '(
    org
    )
  )


(defun gtd/post-init-org ()

  (require 'org-jira)
  ;; Files
  (setq org-directory "~/Dropbox/org")
  ;; (setq org-agenda-files (list "inbox.org" "agenda.org" "notes.org" "projects.org"))
  (setq org-agenda-files
        (mapcar 'file-truename
                (let ((agenda-files (file-expand-wildcards "~/Dropbox/org/*.org")))
                  (if (boundp 'org-jira-working-dir)
                      (append (file-expand-wildcards (concat org-jira-working-dir "/*.org")) agenda-files)
                    ;; (append (directory-files org-jira-working-dir t "^\[^.\].*\\.org$") agenda-files)
                    agenda-files))
                ))
  ;; Capture
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
          ("n" "Note" entry  (file "notes.org")
           ,(concat "* Note (%a)\n"
                    "/Entered on/ %U\n" "\n" "%?"))
          ("@" "Inbox [mu4e]" entry (file "inbox.org")
           ,(concat "* TODO Reply to \"%a\" %?\n"
                    "/Entered on/ %U"))
          ))

  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))

  (defun org-capture-mail ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "@"))

  ;; Use full window for org-capture
  (add-hook 'org-capture-mode-hook 'delete-other-windows)

  ;; Only if you use mu4e
  ;; (require 'mu4e)
  ;; (define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
  ;; (define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

  (spacemacs/set-leader-keys
    "aoi" 'org-capture-inbox
    )

  ;; Refile
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

  ;; TODO
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)")))
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  ;; Agenda
  ;; (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-hide-tags-regexp nil)

  (setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo '("SOMEDAY")))
                        ))
            (todo "TODO" ((org-agenda-files
                           (directory-files org-jira-working-dir t "^\[^.\].*\\.org$"))
                          (org-agenda-overriding-header "\nJIRA\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))))


  ;; Save the corresponding buffers
  (defun gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
			                     (when (member (buffer-file-name) org-agenda-files)
			                       t)))
    (message "Saving org-agenda-files buffers... done"))

  ;; Add it after refile
  (advice-add 'org-refile :after
	            (lambda (&rest _)
	              (gtd-save-org-buffers)))
  )


;;; packages.el ends here
