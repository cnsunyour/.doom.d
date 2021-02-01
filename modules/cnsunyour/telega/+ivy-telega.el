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
(defun ivy-telega-chat-with ()
  "Starts chat with defined peer"
  (interactive)
  (telega t)
  (let ((chats (mapcar
                (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                (telega-filter-chats telega--ordered-chats
                                     (if current-prefix-arg
                                         'all
                                       '(or mention (and unread unmuted)))))))
    (cond ((null chats)
           (user-error "No chats available."))
          ((= 1 (length chats))
           (telega-chat--pop-to-buffer (cdar chats)))
          (t
           (ivy-read "Chat with: " chats
                     :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
                     :caller 'ivy-telega-chat-with)))))

(setq telega-completing-read-function 'ivy-completing-read)

(when (fboundp 'ivy--regex-pinyin)
  (push '(telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist)
  (push '(ivy-telega-chat-with . ivy--regex-pinyin) ivy-re-builders-alist))

(map! "C-c v" #'ivy-telega-chat-with
      "C-c c" (cmd! (let ((current-prefix-arg '(4)))
                      (call-interactively #'ivy-telega-chat-with)))
      "C-c C-SPC" (cmd! (let ((current-prefix-arg '(4)))
                      (call-interactively #'telega-switch-important-chat))))
