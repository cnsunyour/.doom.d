;;; cnsunyour/telega/+telega-ext.el -*- lexical-binding: t; -*-

;; Highlight current line in root buffer
(defun lg-telega-root-mode ()
  (hl-line-mode 1))
(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))
(add-hook 'telega-chat-update-hook 'lg-telega-chat-update)
(add-hook 'telega-root-mode-hook 'lg-telega-root-mode)

;; Outline unmuted chats using braces
(setq telega-chat-button-brackets
      (list (list '(and (not unmuted) (type private))
                  (propertize "{" 'face 'shadow)
                  (propertize "}" 'face 'shadow))
            (list '(and (not unmuted) (type basicgroup))
                  (propertize "(" 'face 'shadow)
                  (propertize ")" 'face 'shadow))
            (list '(and (not unmuted) (type supergroup))
                  (propertize "[" 'face 'shadow)
                  (propertize "]" 'face 'shadow))
            (list '(and (not unmuted) (type channel))
                  (propertize "<" 'face 'shadow)
                  (propertize ">" 'face 'shadow))
            (list '(type private)    "{" "}")
            (list '(type basicgroup) "(" ")")
            (list '(type supergroup) "[" "]")
            (list '(type channel)    "<" ">")
            (list 'all               "[" "]")))

;; Links to chat/message in org-mode
(after! org
  (defun org-telega-follow-link (link)
    (telega-tme-open-tg (concat "tg:telega:" link)))
  (defun org-telega-store-link ()
    (when-let ((link (telega-tme-internal-link-to
                      (or (telega-msg-at (point))
                          (telega-chat-at (point))))))
      ;; NOTE: strip leading "tg:"
      (let ((org-link (substring link 3)))
        (org-link-store-props :type "telega" :link org-link)
        org-link)))
  (defun org-telega-complete-link ()
    (let ((chat (telega-completing-read-chat "Chat: ")))
      (concat "telega:" (number-to-string (plist-get chat :id)))))
  (org-link-set-parameters "telega"
                           :follow 'org-telega-follow-link
                           :store 'org-telega-store-link
                           :complete 'org-telega-complete-link))

;;
;; Send files from dired
;;
(defun telega-dired-attach-func (file)
  "Identify msg type for FILE."
  (let ((file-ext (file-name-extension file)))
    (cond ((member file-ext '("mp3" "flac"))
           #'telega-chatbuf-attach-audio)
          ((member file-ext '("mp4" "mkv"))
           #'telega-chatbuf-attach-video)
          ((image-type-from-file-name file)
           #'telega-chatbuf-attach-photo)
          (t
           #'telega-chatbuf-attach-file))))

(defun telega-dired-attach-send ()
  "Send the marked files."
  (interactive)
  (let ((dired-files (dired-get-marked-files)))
    (unless dired-files
      (user-error "No marked files"))

    (with-current-buffer (telega-chat--pop-to-buffer
                          (telega-completing-read-chat
                           (format "Send %d files to: " (length
                                                         dired-files))))
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (dolist (file dired-files)
          (funcall (telega-dired-attach-func file) file))))))
