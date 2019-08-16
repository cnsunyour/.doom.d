;;;  -*- lexical-binding: t; -*-

;; general keybindings
(map! (:leader
        (:desc "Org Agenda" :gnv "a" #'org-agenda)
        (:desc "CFW Calendar" :gnv "oc" #'cfw:open-org-calendar))

      (:after org
        (:map org-mode-map "C-c o" #'org-pomodoro))
      (:after org-agenda
        (:map org-agenda-mode-map "C-c o" #'org-pomodoro))

      :m "M-j" #'multi-next-line
      :m "M-k" #'multi-previous-line

      ;; Easier window movement
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      (:map vterm-mode-map
        ;; Easier window movement
        :i "C-h" #'evil-window-left
        :i "C-j" #'evil-window-down
        :i "C-k" #'evil-window-up
        :i "C-l" #'evil-window-right)

      (:map evil-treemacs-state-map
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right
        "M-j" #'multi-next-line
        "M-k" #'multi-previous-line)

      (:when IS-LINUX
        "s-x" #'execute-extended-command
        "s-;" #'eval-expression
        ;; use super for window/frame navigation/manipulation
        "s-w" #'delete-window
        "s-W" #'delete-frame
        "s-n" #'+default/new-buffer
        "s-N" #'make-frame
        "s-q" (if (daemonp) #'delete-frame #'evil-quit-all)
        ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
        ;; it imposes some other functionality and overhead we don't need)
        "s-z" #'undo
        "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
        "s-v" #'yank
        "s-s" #'save-buffer
        ;; Buffer-local font scaling
        "s-+" #'doom/reset-font-size
        "s-=" #'doom/increase-font-size
        "s--" #'doom/decrease-font-size
        ;; Conventional text-editing keys
        "s-a" #'mark-whole-buffer
        :gi [s-return]    #'+default/newline-below
        :gi [s-S-return]  #'+default/newline-above
        :gi [s-backspace] #'doom/backward-kill-to-bol-and-indent)

      :leader
      (:prefix "f"
        "t" #'find-in-dotfiles
        "T" #'browse-dotfiles))

;; set initial state to emacs for org-agenda
;; (add-hook! org-agenda-mode
;;   (evil-set-initial-state 'org-agenda-mode 'emacs))


;; smartparens key-binding
(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(bind-keys :map smartparens-mode-map
           ("C-M-a" . sp-beginning-of-sexp)
           ("C-M-e" . sp-end-of-sexp)

           ("C-<down>" . sp-down-sexp)
           ("C-<up>"   . sp-up-sexp)
           ("M-<down>" . sp-backward-down-sexp)
           ("M-<up>"   . sp-backward-up-sexp)

           ("C-M-f" . sp-forward-sexp)
           ("C-M-b" . sp-backward-sexp)

           ("C-M-n" . sp-next-sexp)
           ("C-M-p" . sp-previous-sexp)

           ("C-S-f" . sp-forward-symbol)
           ("C-S-b" . sp-backward-symbol)

           ("C-<right>" . sp-forward-slurp-sexp)
           ("M-<right>" . sp-forward-barf-sexp)
           ("C-<left>"  . sp-backward-slurp-sexp)
           ("M-<left>"  . sp-backward-barf-sexp)

           ("C-M-t" . sp-transpose-sexp)
           ("C-M-k" . sp-kill-sexp)
           ("C-S-k"   . sp-kill-hybrid-sexp)
           ("M-S-k"   . sp-backward-kill-sexp)
           ("C-M-w" . sp-copy-sexp)
           ("C-M-d" . delete-sexp)

           ("M-<backspace>" . backward-kill-word)
           ("C-<backspace>" . sp-backward-kill-word)
           ([remap sp-backward-kill-word] . backward-kill-word)

           ("M-[" . sp-backward-unwrap-sexp)
           ("M-]" . sp-unwrap-sexp)

           ("C-x C-t" . sp-transpose-hybrid-sexp)

           ("C-x ("  . wrap-with-parens)
           ("C-x ["  . wrap-with-brackets)
           ("C-x {"  . wrap-with-braces)
           ("C-x '"  . wrap-with-single-quotes)
           ("C-x \"" . wrap-with-double-quotes)
           ("C-x _"  . wrap-with-underscores)
           ("C-x `"  . wrap-with-back-quotes))
