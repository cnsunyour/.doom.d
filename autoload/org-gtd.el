;;; config/private/autoload/org-gtd.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-gtd/find-project-task ()
  "Move point to the parent (project) task if any"
  (interactive)
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (string-match-p "PROJECT" (org-get-tags-string))
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;;;###autoload
(defun +org-gtd/is-project-subtree-p ()
  "Any task that is a project subtree"
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (+org-gtd/find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun +org-gtd/skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG. If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

;;;###autoload
(defun +org-gtd/skip-project-tasks ()
  "Skip tasks belonging to projects"
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((+org-gtd/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun +org-gtd/tag-task-started ()
  "Add started tag to task"
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (if (not (member "started" (split-string (org-get-tags-string) ":")))
        (org-set-tags-to (concat (org-get-tags-string) ":started:"))
      (org-set-tags t))))

;;;###autoload
(defun +org-gtd/remove-started-tag-when-done ()
  "Remove started tag from task when marked as DONE"
  (interactive)
  (when (org-entry-is-done-p)
    (save-excursion
      (org-back-to-heading)
      (org-set-tags-to (if (string= (org-get-tags-string) ":started:")
                           ""
                         (mapconcat 'identity
                                    (delete "started" (split-string (org-get-tags-string) ":")) ":"))))))

;;;###autoload
(defun +org-gtd/clock-in-to-next (&optional param)
  "Switch a task from TODO or NEXT to STARTED when clocking in"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (if (member (org-get-todo-state) (list "TODO" "NEXT")) "STARTED")))

;;;###autoload
(defun +org-gtd/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords))
   (member "REFILE" (split-string (org-get-tags-string) ":"))))

;;;###autoload
(defun +org-gtd/archive-all-done-entries ()
  "Archive all entries marked DONE"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (org-entry-is-done-p)
        (org-archive-subtree)))))

;;;###autoload
(defun +org-gtd/delete-all-done-entries ()
  "Archive all entries marked DONE"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (org-entry-is-done-p)
        (org-cut-subtree)))))

