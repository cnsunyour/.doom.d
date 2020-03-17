;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (funcall (telega--tl-prop :unread_count) chat))
        (title (telega-chat-title chat 'with-identity))
        (not-muted-p (not (telega-chat-muted-p chat)))
        (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

    (if (and not-muted-p (> (+ unread mentions) 0))
        (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
      title)))

;;;###autoload
(defun ivy-telega-chat-with ()
  "Starts chat with defined peer"
  (interactive)

  (telega t)
  (let ((chats (mapcar
                (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                (telega-filter-chats telega--ordered-chats 'all))))
    (ivy-read "chat: " chats
              :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
              :caller 'ivy-telega-chat-with)))

(setq telega-completing-read-function 'ivy-completing-read)

(when (fboundp 'ivy--regex-pinyin)
  (push '(telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist)
  (push '(ivy-telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist))

(map! "C-c v" #'ivy-telega-chat-with
      "C-M-S-s-c" #'ivy-telega-chat-with)
