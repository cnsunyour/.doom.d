;;; cnsunyour/telega/+telega-auto-input-method.el -*- lexical-binding: t; -*-


(defcustom +telega--chat-cn-list nil
  "`telega' 里中文对话列表，用于自动切换到中文输入。"
  :type 'list
  :group 'telega)

(defcustom +telega--chat-en-list nil
  "`telega' 里英文对话列表，用于自动切换到英文输入。"
  :type 'list
  :group 'telega)

(defun +telega--save-encn-list ()
  "保存中英文对话列表。"
  (customize-save-variable '+telega--chat-cn-list +telega--chat-cn-list)
  (customize-save-variable '+telega--chat-en-list +telega--chat-en-list))

(defun +telega--add-cn-list (chat)
  "将当前聊天对话加入到中文列表中，以自动激活中文输入法。"
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at (point)))))
  (let ((chatid (plist-get chat :id)))
    (cl-pushnew chatid +telega--chat-cn-list)
    (cl-remove chatid +telega--chat-en-list)
    (+telega--save-encn-list)))

(defun +telega--add-en-list (chat)
  "将当前聊天对话加入到英文列表中，以自动关闭中文输入法。"
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at (point)))))
  (let ((chatid (plist-get chat :id)))
    (cl-pushnew chatid +telega--chat-en-list)
    (cl-remove chatid +telega--chat-cn-list)
    (telega--save-encn-list)))

(defun +telega--remove-from-list (chat)
  "将当前聊天对话从中英文列表中删除，将根据对话名称自动切换中英文输入法。"
  (interactive (list (or telega-chatbuf--chat
                         telega--chat
                         (telega-chat-at (point)))))
  (let ((chatid (plist-get chat :id)))
    (cl-remove chatid +telega--chat-en-list)
    (cl-remove chatid +telega--chat-cn-list)
    (telega--save-encn-list)))

(defadvice! +toggle-input-method--telega-chat-mode-a (chat &optional no-history-load)
  "在 telega-chat-mode 里根据 chat 名称切换输入法，如果名称包含
中文，则激活中文输入法，否则关闭中文输入法"
  :after #'telega-chat--pop-to-buffer
  (let ((input-method default-input-method)
        (title (telega-chat-title chat))
        (chatid (plist-get chat :id)))
    (cond ((member chatid +telega--chat-cn-list) (activate-input-method input-method))
          ((member chatid +telega--chat-en-list) (activate-input-method nil))
          ((string-match "\\cc" title) (activate-input-method input-method))
          ((telega-chat-bot-p chat) (activate-input-method nil))
          ((telega-chat-private-p chat) (activate-input-method input-method))
          (t (activate-input-method nil)))))
