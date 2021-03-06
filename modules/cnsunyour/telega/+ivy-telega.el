;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (funcall (telega--tl-prop :unread_count) chat))
        (title (telega-chat-title-with-brackets chat 'with-identity))
        (not-muted-p (not (telega-chat-muted-p chat)))
        (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))
    (if (and not-muted-p (> (+ unread mentions) 0))
        (concat (ivy-append-face (format "%s" title) 'ivy-highlight-face)
                "("
                (ivy-append-face (format "%d" unread) 'telega-unread-unmuted-modeline)
                (when (> mentions 0)
                  (ivy-append-face (format "@%d" mentions) 'telega-mention-count))
                ")")
      title)))

;;;###autoload
(defun ivy-telega-chat-with (&optional prefix-u)
  "Starts chat with defined peer"
  (interactive "p")
  (telega t)
  (let ((chats (mapcar
                (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                (telega-filter-chats telega--ordered-chats
                                     (if (= 4 prefix-u)
                                         '(or main archive)
                                       '(and (or mention (and unread unmuted))
                                             (or main archive)))))))
    (cond ((null chats)
           (user-error "No chats available."))
          ((or (= 1 (length chats)) (= 16 prefix-u))
           (telega-chat--pop-to-buffer (cdar chats)))
          (t
           (ivy-read "Chat with: " chats
                     :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
                     :caller 'ivy-telega-chat-with)))))

(setq telega-completing-read-function 'ivy-completing-read)

(when (fboundp 'ivy--regex-pinyin)
  (push '(telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist)
  (push '(ivy-telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist))

(map! :desc "Telega important chats"
      "C-c v" #'ivy-telega-chat-with
      :desc "Telega all chats"
      "C-c c" (cmd! (let ((current-prefix-arg '(4)))
                      (call-interactively #'ivy-telega-chat-with)))
      :desc "Telega next important chat"
      "C-c C-SPC" (cmd! (let ((current-prefix-arg '(16)))
                          (call-interactively #'ivy-telega-chat-with))))
