;;; highlight-matching-tag.el --- Highlight matching tag

;; Filename: highlight-matching-tag.el
;; Description: Highlight matching tag
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-03-14 22:14:00
;; Version: 0.3
;; Last-Updated: 2019-06-28 19:46:30
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/highlight-matching-tag.el
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
;; This plugin will highlight matching tag instantaneously.
;;

;;; Installation:
;;
;; Put highlight-matching-tag.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'highlight-matching-tag)
;; (highlight-matching-tag 1)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET highlight-matching-tag RET
;;

;;; Change log:
;;
;; 2019/06/28
;;      * Unmark tag overlay when cursor under comment.
;;      * Fix error of `highlight-matching-tag-get-close-tag-bound'
;;
;; 2019/06/27
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

(defgroup highlight-matching-tag nil
  "Highlight matching tag."
  :group 'highlight-matching-tag)

(defface highlight-matching-tag-mark-face
  '((t (:underline "#DAB645")))
  "Face tag."
  :group 'highlight-matching-tag)

(defun highlight-matching-tag-monitor-cursor-move ()
  (when (derived-mode-p 'web-mode)
    (unless (boundp 'highlight-matching-tag-cursor-position)
      (set (make-local-variable 'highlight-matching-tag-cursor-position) -1))

    (unless (equal (point) 'highlight-matching-tag-cursor-position)
      (cond
       ((highlight-matching-tag-in-comment-p)
        (highlight-matching-tag-unmark))
       ((highlight-matching-tag-in-open-tag-p)
        (highlight-matching-tag-mark))
       ((highlight-matching-tag-in-close-tag-p)
        (highlight-matching-tag-mark))
       (t
        (highlight-matching-tag-unmark))))

    (set (make-local-variable 'highlight-matching-tag-cursor-position) (point))))

(defun highlight-matching-tag (&optional arg)
  (if (> arg 0)
      (add-hook 'post-command-hook #'highlight-matching-tag-monitor-cursor-move)
    (remove-hook 'post-command-hook #'highlight-matching-tag-monitor-cursor-move)))

(defun highlight-matching-tag-is-marking ()
  (and (boundp 'highlight-matching-tag-is-mark)
       highlight-matching-tag-is-mark))

(defun highlight-matching-tag-mark ()
  (highlight-matching-tag-unmark)

  (let* ((open-tag-bound (highlight-matching-tag-get-open-tag-bound)))
    (if open-tag-bound
        (let* ((start-pos (nth 0 open-tag-bound))
               (end-pos (nth 1 open-tag-bound)))
          (set (make-local-variable 'highlight-matching-tag-open-overlay) (make-overlay start-pos end-pos))
          (overlay-put highlight-matching-tag-open-overlay 'face 'highlight-matching-tag-mark-face))
      (set (make-local-variable 'highlight-matching-tag-open-overlay) nil)))

  (let ((close-tag-bound (highlight-matching-tag-get-close-tag-bound)))
    (if close-tag-bound
        (let* ((start-pos (nth 0 close-tag-bound))
               (end-pos (nth 1 close-tag-bound)))
          (set (make-local-variable 'highlight-matching-tag-close-overlay) (make-overlay start-pos end-pos))
          (overlay-put highlight-matching-tag-close-overlay 'face 'highlight-matching-tag-mark-face))
      (set (make-local-variable 'highlight-matching-tag-close-overlay) nil))))

(defun highlight-matching-tag-unmark ()
  (when (and (boundp 'highlight-matching-tag-open-overlay)
             highlight-matching-tag-open-overlay)
    (delete-overlay highlight-matching-tag-open-overlay)
    (set (make-local-variable 'highlight-matching-tag-open-overlay) nil))

  (when (and (boundp 'highlight-matching-tag-close-overlay)
             highlight-matching-tag-close-overlay)
    (delete-overlay highlight-matching-tag-close-overlay)
    (set (make-local-variable 'highlight-matching-tag-close-overlay) nil)))

(defun highlight-matching-tag-in-open-tag-p ()
  (ignore-errors
    (let ((open-tag-pos (save-excursion
                          (sgml-skip-tag-backward 1)
                          (point))))
      (and (equal (line-number-at-pos open-tag-pos) (line-number-at-pos (point)))
           (save-excursion
             (search-backward-regexp "<" open-tag-pos t))
           (not (save-excursion
                  (search-backward-regexp "\\s-" open-tag-pos t)))))))

(defun highlight-matching-tag-in-close-tag-p ()
  (ignore-errors
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
                  (search-backward-regexp "\\s-" close-tag-pos t)))))))

(defun highlight-matching-tag-get-open-tag-bound ()
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

(defun highlight-matching-tag-get-close-tag-bound ()
  (ignore-errors
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
          )))))

(defun highlight-matching-tag-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun highlight-matching-tag-in-comment-p (&optional state)
  (save-excursion
    (or (nth 4 (or state (highlight-matching-tag-current-parse-state)))
        (eq (get-text-property (point) 'face) 'font-lock-comment-face)
        (eq (get-text-property (point) 'face) 'web-mode-comment-face))))

(provide 'highlight-matching-tag)

;;; highlight-matching-tag.el ends here
