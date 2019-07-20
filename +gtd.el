;;;  -*- lexical-binding: t; -*-

;; Sets up a GTD workflow using Org-mode
;;
;; Mostly taken from Brent Hansen's setup
;; http://doc.norang.ca/org-mode.html
;; and kandread's doom-emacs config
;; https://github.com/kandread/doom-emacs-private

(after! org
  ;; set org file directory
  (setq org-directory "~/Dropbox/org/")
  (setq org-gtd-directory "~/Dropbox/gtd/")
  ;; set agenda files
  (setq org-agenda-files (list org-gtd-directory))
  ;; set task states
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(y@/!)" "|" "ABORT(a@/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "orange" :weight bold)
          ("NEXT" :foreground "yellow" :weight bold)
          ("STARTED" :foreground "white" :weight bold)
          ("WAITING" :foreground "brown" :weight bold)
          ("SOMEDAY" :foreground "purple" :weight bold)
          ("DONE" :foreground "green" :weight bold)
          ("ABORT" :foreground "red" :weight bold)))
  (setq org-tag-alist
        '(("FLAGGED" . ?f)
          ("@Office" . ?o)
          ("@Home" . ?h)
          ("@Way" . ?w)
          ("@Computer" . ?c)
          ("@Errands" . ?e)
          ("@Lunchtime" . ?l)))
  (setq org-tag-persistent-alist
        org-tag-alist)
  (setq org-priority-faces
        '((?A . (:foreground "red"))
          (?B . (:foreground "yellow"))
          (?C . (:foreground "green"))))

  ;; trigger task states
  (setq org-todo-state-tags-triggers
        (quote (("ABORT" ("ABORT" . t))
                ("WAITING" ("SOMEDAY") ("WAITING" . t))
                ("SOMEDAY" ("WAITING") ("SOMEDAY" . t))
                (done ("WAITING") ("SOMEDAY"))
                ("TODO" ("WAITING") ("ABORT") ("SOMEDAY"))
                ("NEXT" ("WAITING") ("ABORT") ("SOMEDAY"))
                ("STARTED" ("WAITING") ("ABORT") ("SOMEDAY"))
                ("DONE" ("WAITING") ("ABORT") ("SOMEDAY")))))
  ;; exclude PROJECT tag from being inherited
  (setq org-tags-exclude-from-inheritance '("PROJECT"))
  ;; show inherited tags in agenda view
  (setq org-agenda-show-inherited-tags t)
  ;; set default notes file
  (setq org-default-notes-file (expand-file-name "inbox.org" org-gtd-directory))
  (setq org-agenda-file-gtd (expand-file-name "inbox.org" org-gtd-directory))
  (setq org-agenda-file-note (expand-file-name "note.org" org-directory))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
  ;; set capture templates
  (setq org-capture-templates
        '(("i" "New Todo Task" entry (file+headline org-agenda-file-gtd "Tasks")
           "* TODO [#B] %^{Todo Topic}\n:PROPERTIES:\n:Created: %U\n:END:"
           :prepend t :clock-in t :clock-resume t :empty-lines-before 0 :empty-lines-after 1)
          ("n" "Taking Notes" entry (file+olp+datetree org-agenda-file-note)
           "* %^{Notes Topic}\n:PROPERTIES:\n:Created: %U\n:END:\n　%?"
           :prepend t :clock-in t :clock-resume t :empty-lines-before 0 :empty-lines-after 1)
          ("j" "Keeping Journals" entry (file+olp+datetree org-agenda-file-journal)
           "* %^{Journal Topic}\n:PROPERTIES:\n:Created: %U\n:END:\n　%?"
           :prepend t :clock-in t :clock-resume t :empty-lines-before 0 :empty-lines-after 1)))
  ;; set archive tag
  (setq org-archive-tag "ARCHIVE")
  ;; set archive file
  (setq org-archive-location "::* Archived Tasks")
  ;; refiling targets include any file contributing to the agenda - up to 2 levels deep
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :level . 1)))
  ;; show refile targets simultaneously
  (setq org-outline-path-complete-in-steps nil)
  ;; use full outline paths for refile targets
  (setq org-refile-use-outline-path 'file)
  ;; allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; exclude done tasks from refile targets
  (setq org-refile-target-verify-function #'+org-gtd/verify-refile-target)
  ;; include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))
  ;; resume clocking when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state #'+org-gtd/clock-in-to-next)
  ;; separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; show agenda as the only window
  (setq org-agenda-window-setup 'current-window)
  ;; define stuck projects
  (setq org-stuck-projects '("+LEVEL=2/-DONE-ABORT" ("TODO" "NEXT" "STARTED") ("@Launchtime") "\\<IGNORE\\>"))
  ;; perform actions before finalizing agenda view
  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (setq appt-message-warning-time 10        ;; warn 10 min in advance
                    appt-display-diary nil              ;; do not display diary when (appt-activate) is called
                    appt-display-mode-line t            ;; show in the modeline
                    appt-display-format 'window         ;; display notification in window
                    calendar-mark-diary-entries-flag t) ;; mark diary entries in calendar
              (org-agenda-to-appt)                      ;; copy all agenda schedule to appointments
              (appt-activate 1)))
  ;; exclude archived tasks from agenda view
  (setq org-agenda-tag-filter-preset '("-ARCHIVE"))
  ;; disable compact block agenda view
  (setq org-agenda-compact-blocks nil)
  ;; block tasks that have unfinished subtasks
  (setq org-enforce-todo-dependencies t)
  ;; dont't dim blocked tasks in agenda
  (setq org-agenda-dim-blocked-tasks nil)
  ;; inhibit startup when preparing agenda buffer
  (setq org-agenda-inhibit-startup nil)
  ;; limit number of days before showing a future deadline
  (setq org-deadline-warning-days 7)
  ;; Number of days to include in overview display.
  (setq org-agenda-span 'week)
  ;; retain ignore options in tags-todo search
  (setq org-agenda-tags-todo-honor-ignore-options t)
  ;; hide certain tags from agenda view
  (setq org-agenda-hide-tags-regexp
        (regexp-opt '("PROJECT" "REFILE" "NOTE" "JOURNAL")))
  ;; remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)
  ;; remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)
  ;; remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)
  ;; skip scheduled delay when entry also has a deadline.
  (setq org-agenda-skip-scheduled-delay-if-deadline t)
  ;; 设置超过Headline的重复任务不再显示
  (setq org-agenda-skip-scheduled-if-deadline-is-shown 'repeated-after-deadline)
  ;; include entries from the Emacs diary
  (setq org-agenda-include-diary t)
  ;; custom diary file to org-ddirectory
  (setq diary-file (expand-file-name "diary" org-directory))
  ;; 使用最后的clock-out时间作为条目关闭时间
  (setq org-use-last-clock-out-time-as-effective-time t)
  ;; 设置为DONE或ABORT状态时，会生成CLOSED时间戳
  (setq org-log-done 'time)
  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)
  ;; custom agenda commands
  (setq org-agenda-custom-commands
        '(("r" "Archivable" todo "DONE|ABORT"
           ((org-agenda-overriding-header "Tasks to Archive:")
            (org-tags-match-list-sublevels nil)))
          ("f" "Flagged" tags-todo "+FLAGGED/!"
           ((org-agenda-overriding-header "Flagged Tasks:")
            (org-tags-match-list-sublevels t)))
          ("p" "Projects" tags-todo "+PROJECT-LEVEL=1/!"
           ((org-agenda-overriding-header "Project Tasks:")
            (org-tags-match-list-sublevels 'indented)))
          ("n" "Notes" tags "+NOTE-LEVEL=1-LEVEL=2-LEVEL=3"
           ((org-agenda-files (list org-agenda-file-note))
            (org-agenda-overriding-header "Notes:")
            (org-tags-match-list-sublevels t)))
          ("j" "Journals" tags "+JOURNAL-LEVEL=1-LEVEL=2-LEVEL=3"
           ((org-agenda-files (list org-agenda-file-journal))
            (org-agenda-overriding-header "Journals:")
            (org-tags-match-list-sublevels t)))
          (" " "<SPC> Awesome Agenda View"
           ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
                        (org-agenda-show-log t)
                        (org-agenda-log-mode-items '(clock state))
                        (org-agenda-span 'day)
                        (org-agenda-start-day "+0d")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-todo-ignore-deadlines nil)))
            (tags-todo "-ABORT/!NEXT|STARTED"
                       ((org-agenda-overriding-header "Next and Active Tasks:")))
            (agenda "" ((org-agenda-overriding-header "Upcoming Deadlines:")
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-span 'day)
                        (org-agenda-start-day "+0d")
                        (org-agenda-start-on-weekday nil)
                        (org-deadline-warning-days 30)
                        (org-agenda-time-grid nil)))
            (agenda "" ((org-agenda-overriding-header "Week at a Glance:")
                        (org-agenda-span 5)
                        (org-agenda-start-day "+1d")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-time-grid nil)
                        (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))))
            (tags "+REFILE-LEVEL=1"
                  ((org-agenda-overriding-header "Tasks to Refile:")
                   (org-tags-match-list-sublevels nil)))
            (org-agenda-list-stuck-projects)
            (tags-todo "-REFILE-PROJECT-ABORT/!"
                       ((org-agenda-overriding-header "Standalone Tasks:")
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-agenda-todo-ignore-timestamp t)
                        (org-agenda-todo-ignore-with-date t)))
            )
           ;; ((org-agenda-compact-blocks t))
           )
          ))

  (defun gtd-inbox() (interactive) (find-file org-agenda-file-gtd))
  (global-set-key (kbd "C-c i") 'gtd-inbox)
  (defun gtd-note() (interactive) (find-file org-agenda-file-note))
  (global-set-key (kbd "C-c n") 'gtd-note)
  (defun gtd-journal() (interactive) (find-file org-agenda-file-journal))
  (global-set-key (kbd "C-c j") 'gtd-journal)

  ;; open files with default apps
  (setq org-file-apps
        '(("\\.docx\\'" . default)
          ("\\.pdf\\'" . default)
          ("\\.png\\'" . default)
          ("\\.odt\\'" . default)
          (auto-mode . emacs)))
  )
