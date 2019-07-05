;;; instant-rename-tag.el --- Instant rename tag

;; Filename: instant-rename-tag.el
;; Description: Instant rename tag
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-03-14 22:14:00
;; Version: 0.6
;; Last-Updated: 2019-06-27 20:07:21
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/instant-rename-tag.el
;; Keywords:
;; Compatibility: GNU Emacs 26.1.92
;;
;; Features that might be required by this library:
;;
;; `web-mode' `sgml-mode'

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; It's wonderful if we can rename tag instantly, not rename from minibuffer.
;; And yes, this plugin is design for do this.
;;

;;; Installation:
;;
;; Put instant-rename-tag.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'instant-rename-tag)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET instant-rename-tag RET
;;

;;; Change log:
;;
;; 2019/06/27
;;      * Refactory code.
;;      * Fix error when no close tag exists.
;;      * Adjust open overlay bound if not close tag.
;;      * Automatically cancel tag mark when editing non-tag area
;;      * Refactory code for clear logic.
;;
;; 2019/06/26
;;      * Use overlay re-implement code.
;;
;; 2019/03/14
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;

;;; Require
(require 'web-mode)
(require 'sgml-mode)

;;; Code:

(defgroup instant-rename-tag nil
  "Instant rename tag."
  :group 'instant-rename-tag)

(defface instant-rename-tag-mark-face
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face tag."
  :group 'instant-rename-tag)

(defun instant-rename-tag ()
  (interactive)
  (cond ((instant-rename-tag-in-open-tag-p)
         (if (instant-rename-tag-is-marking)
             (instant-rename-tag-unmark)
           (instant-rename-tag-mark)))
        ((instant-rename-tag-in-close-tag-p)
         (if (instant-rename-tag-is-marking)
             (instant-rename-tag-unmark)
           (instant-rename-tag-mark)))
        (t
         (message "Not in tag area."))))

(defun instant-rename-tag-is-marking ()
  (and (boundp 'instant-rename-tag-is-mark)
       instant-rename-tag-is-mark))

(defun instant-rename-tag-mark ()
  (ignore-errors
    (let* ((open-tag-bound (instant-rename-tag-get-open-tag-bound)))
      (if open-tag-bound
          (let* ((start-pos (nth 0 open-tag-bound))
                 (end-pos (nth 1 open-tag-bound)))
            (set (make-local-variable 'instant-rename-tag-open-overlay) (make-overlay start-pos end-pos))
            (overlay-put instant-rename-tag-open-overlay 'face 'instant-rename-tag-mark-face))
        (set (make-local-variable 'instant-rename-tag-open-overlay) nil))))

  (ignore-errors
    (let ((close-tag-bound (instant-rename-tag-get-close-tag-bound)))
      (if close-tag-bound
          (let* ((start-pos (nth 0 close-tag-bound))
                 (end-pos (nth 1 close-tag-bound)))
            (set (make-local-variable 'instant-rename-tag-close-overlay) (make-overlay start-pos end-pos))
            (overlay-put instant-rename-tag-close-overlay 'face 'instant-rename-tag-mark-face))
        (set (make-local-variable 'instant-rename-tag-close-overlay) nil))))

  (set (make-local-variable 'instant-rename-tag-is-mark) t))

(defun instant-rename-tag-unmark ()
  (when instant-rename-tag-open-overlay
    (delete-overlay instant-rename-tag-open-overlay)
    (set (make-local-variable 'instant-rename-tag-open-overlay) nil))

  (when instant-rename-tag-close-overlay
    (delete-overlay instant-rename-tag-close-overlay)
    (set (make-local-variable 'instant-rename-tag-close-overlay) nil))

  (set (make-local-variable 'instant-rename-tag-is-mark) nil))

(defun instant-rename-tag-in-open-tag-p ()
  (let ((open-tag-pos (save-excursion
                        (sgml-skip-tag-backward 1)
                        (point))))
    (and (equal (line-number-at-pos open-tag-pos) (line-number-at-pos (point)))
         (save-excursion
           (search-backward-regexp "<" open-tag-pos t))
         (not (save-excursion
                (search-backward-regexp "\\s-" open-tag-pos t))))))

(defun instant-rename-tag-in-close-tag-p ()
  (let ((close-tag-pos
         (save-excursion
           (web-mode-element-beginning)
           (cond ((looking-at "<>")
                  (web-mode-tag-match))
                 (t
                  (sgml-skip-tag-forward 1)))
           (search-backward-regexp "</")
           (point))))
    (and (equal (line-number-at-pos close-tag-pos) (line-number-at-pos (point)))
         (save-excursion
           (search-backward-regexp "</" close-tag-pos t))
         (not (save-excursion
                (search-backward-regexp "\\s-" close-tag-pos t))))))

(defun instant-rename-tag-get-open-tag-bound ()
  (let* ((open-tag-pos (save-excursion
                         (web-mode-element-beginning)
                         (point)))
         (start-pos (save-excursion
                      (goto-char open-tag-pos)
                      (forward-char 1)
                      (point)))
         (end-pos (save-excursion
                    (goto-char start-pos)
                    (unless (looking-at ">")
                      (forward-symbol 1))
                    (point))))
    (if (and start-pos end-pos)
        (list start-pos end-pos)
      nil)))

(defun instant-rename-tag-get-close-tag-bound ()
  (save-excursion
    (let (open-tag-start-pos
          close-tag-start-pos
          close-tag-end-pos)
      ;; Move cursor to open tab beginning point.
      (web-mode-element-beginning)
      ;; Record beginning point.
      (setq open-tag-start-pos (point))
      ;; Jump to end tag.
      (cond ((looking-at "<>")
             (web-mode-tag-match))
            (t
             (sgml-skip-tag-forward 1)))
      (search-backward-regexp "</")

      (if (<= (point) open-tag-start-pos)
          ;; Return nil if end tag point small than start tag beginning point.
          nil
        ;; Otherwise return close tag bound.
        (setq close-tag-start-pos
              (save-excursion
                (forward-char 2)
                (point)))
        (setq close-tag-end-pos
              (save-excursion
                (goto-char close-tag-start-pos)
                (unless (looking-at ">")
                  (forward-symbol 1))
                (point)))
        (list close-tag-start-pos close-tag-end-pos)
        ))))

(defun instant-rename-tag-sync-close-tag ()
  (let* ((open-tag-start-pos (overlay-start instant-rename-tag-open-overlay))
         (open-tag-end-pos (overlay-end instant-rename-tag-open-overlay))
         (close-tag-start-pos (overlay-start instant-rename-tag-close-overlay))
         (close-tag-end-pos (overlay-end instant-rename-tag-close-overlay))
         (new-tag (buffer-substring open-tag-start-pos (max open-tag-end-pos (point)))))
    (save-excursion
      (delete-region close-tag-start-pos close-tag-end-pos)
      (goto-char close-tag-start-pos)
      (insert new-tag)
      (move-overlay instant-rename-tag-open-overlay open-tag-start-pos (+ open-tag-start-pos (length new-tag)))
      (move-overlay instant-rename-tag-close-overlay (- (point) (length new-tag)) (point))
      )))

(defun instant-rename-tag-sync-open-tag ()
  (let* ((open-tag-start-pos (overlay-start instant-rename-tag-open-overlay))
         (open-tag-end-pos (overlay-end instant-rename-tag-open-overlay))
         (close-tag-start-pos (overlay-start instant-rename-tag-close-overlay))
         (close-tag-end-pos (overlay-end instant-rename-tag-close-overlay))
         (open-tag (buffer-substring open-tag-start-pos open-tag-end-pos))
         (current-point (max close-tag-end-pos (point)))
         (new-tag (buffer-substring close-tag-start-pos current-point))
         (tag-offset (- (length new-tag) (length open-tag)))
         (close-tag-new-start-pos (+ close-tag-start-pos tag-offset)))
    (save-excursion
      (delete-region open-tag-start-pos open-tag-end-pos)
      (goto-char open-tag-start-pos)
      (insert new-tag)
      (move-overlay instant-rename-tag-close-overlay close-tag-new-start-pos (+ close-tag-new-start-pos (length new-tag)))
      (move-overlay instant-rename-tag-open-overlay (- (point) (length new-tag)) (point)))))

(defun instant-rename-tag-in-open-tag-overlay ()
  (let ((open-tag-start-pos (overlay-start instant-rename-tag-open-overlay))
        (open-tag-end-pos (overlay-end instant-rename-tag-open-overlay)))
    (and (>= (point) open-tag-start-pos)
         (<= (point) (+ 1 open-tag-end-pos)))))

(defun instant-rename-tag-in-close-tag-overlay ()
  (let ((close-tag-start-pos (overlay-start instant-rename-tag-close-overlay))
        (close-tag-end-pos (overlay-end instant-rename-tag-close-overlay)))
    (and (>= (point) close-tag-start-pos)
         (<= (point) (+ 1 close-tag-end-pos)))))

(defun instant-rename-tag-update-open-tag-bound ()
  (let* ((open-tag-start-pos (overlay-start instant-rename-tag-open-overlay))
         (open-tag-end-pos (overlay-end instant-rename-tag-open-overlay))
         (new-tag (buffer-substring open-tag-start-pos (max open-tag-end-pos (point)))))
    (save-excursion
      (move-overlay instant-rename-tag-open-overlay open-tag-start-pos (+ open-tag-start-pos (length new-tag)))
      )))

(defun instant-rename-tag-after-change-function (begin end length)
  (when (and
         (derived-mode-p 'web-mode)
         (instant-rename-tag-is-marking))
    (let ((company-mode-is-active
           (and (featurep 'company-mode)
                company-mode)))
      ;; Temp turn off company-mode if company mode is active.
      (when company-mode-is-active
        (company-mode -1))
      (cond
       ;; Sync tag content if open tag and close tag all exists.
       ((and instant-rename-tag-open-overlay
             instant-rename-tag-close-overlay)
        (cond
         ;; Sync content to close tag if cursor in open tag area.
         ((instant-rename-tag-in-open-tag-overlay)
          (instant-rename-tag-sync-close-tag))
         ;; Sync content to open tag if cursor in close tag area.
         ((instant-rename-tag-in-close-tag-overlay)
          (instant-rename-tag-sync-open-tag))
         ;; Otherwise unmark tag area.
         (t
          (instant-rename-tag-unmark))
         ))
       ;; Update open tag overlay bound if just have open tag.
       (instant-rename-tag-open-overlay
        (cond
         ;; Update open tag overlay bound if cursor in open tag area.
         ((instant-rename-tag-in-open-tag-overlay)
          (instant-rename-tag-update-open-tag-bound))
         ;; Otherwise unmark tag area.
         (t
          (instant-rename-tag-unmark))
         )))
      ;; Turn on company-mode if company mode is active before rename tag action.
      (when company-mode-is-active
        (company-mode 1))
      )))

(add-hook 'after-change-functions #'instant-rename-tag-after-change-function)

(provide 'instant-rename-tag)

;;; instant-rename-tag.el ends here
