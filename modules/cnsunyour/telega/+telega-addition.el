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


;;; Setup a filter for watching some telega chatbufs.
;;
(defcustom +telega--chat-on-watching-list nil
  "List of watching chatbuf."
  :type 'list
  :group 'telega)

(define-telega-filter +chat-on-watching (chat)
  "Check if current chatbuf is on watching."
  (let ((chatid (plist-get chat :id)))
    (member chatid +telega--chat-on-watching-list)))

(setq telega-chat-show-deleted-messages-for '(+chat-on-watching))
