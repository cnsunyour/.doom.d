;;;  -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;; (when (modulep! :ui workspaces)
;;   (global-set-key (kbd "s-[") #'+workspace/switch-left)
;;   (global-set-key (kbd "s-]") #'+workspace/switch-right)
;;   (define-key key-translation-map (kbd "s-1") (kbd "M-1"))
;;   (define-key key-translation-map (kbd "s-2") (kbd "M-2"))
;;   (define-key key-translation-map (kbd "s-3") (kbd "M-3"))
;;   (define-key key-translation-map (kbd "s-4") (kbd "M-4"))
;;   (define-key key-translation-map (kbd "s-5") (kbd "M-5"))
;;   (define-key key-translation-map (kbd "s-6") (kbd "M-6"))
;;   (define-key key-translation-map (kbd "s-7") (kbd "M-7"))
;;   (define-key key-translation-map (kbd "s-8") (kbd "M-8"))
;;   (define-key key-translation-map (kbd "s-9") (kbd "M-9"))
;;   (define-key key-translation-map (kbd "s-0") (kbd "M-0")))

;; add keybindings for dired+ranger
(when (modulep! :emacs dired +ranger)
  (setq ranger-key [?\C-\s-p]))

;; add hydra keybindings for evil-mc
(when (and (modulep! :editor evil)
           (modulep! :editor multiple-cursors))
  (defhydra hydra-evil-mc (:color blue :hint nil)
    "
 _M_ all match          _m_ here           _u_ undo
 _n_ next match         _j_ next line      _s_ suspend
 _p_ prev match         _k_ previous line  _r_ resume
 _N_ skip & next match  _H_ first cursor   _q_ quit
 _P_ skip & prev match  _L_ last cursor    _O_ quit
    "
    ("m" evil-mc-make-cursor-here :exit nil)
    ("M" evil-mc-make-all-cursors :exit nil)
    ("n" evil-mc-make-and-goto-next-match :exit nil)
    ("p" evil-mc-make-and-goto-prev-match :exit nil)
    ("N" evil-mc-skip-and-goto-next-match :exit nil)
    ("P" evil-mc-skip-and-goto-prev-match :exit nil)
    ("j" evil-mc-make-cursor-move-next-line :exit nil)
    ("k" evil-mc-make-cursor-move-prev-line :exit nil)
    ("H" evil-mc-make-and-goto-first-cursor :exit nil)
    ("L" evil-mc-make-and-goto-last-cursor :exit nil)
    ("u" evil-mc-undo-last-added-cursor :exit nil)
    ("r" evil-mc-resume-cursors)
    ("s" evil-mc-pause-cursors)
    ("O" evil-mc-undo-all-cursors)
    ("q" evil-mc-undo-all-cursors))

  (evil-define-key* '(normal visual) 'global
    (kbd "M") 'hydra-evil-mc/body)

  (evil-define-key* 'visual evil-mc-key-map
    "A" 'evil-mc-make-cursor-in-visual-selection-end
    "I" 'evil-mc-make-cursor-in-visual-selection-beg))

;; general keybindings
(map! (:leader (:prefix "b" "h" #'+doom-dashboard/open))

      :g "C-!" #'kill-buffer-and-window
      :g "C-s-S-l" #'toggle-truncate-lines
      :g "M-o" #'next-window-any-frame
      :g "M-O" #'previous-window-any-frame

      (:when (modulep! :ui workspaces)
        "C-M-S-s-w" #'=calendar)

      (:when (modulep! :app rss)
        "C-M-S-s-r" #'=rss)

      (:when (modulep! :email mu4e)
        "C-M-S-s-e" #'=mu4e)

      (:when (modulep! :email notmuch)
        "C-M-S-s-e" #'=notmuch)

      (:map evil-treemacs-state-map
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
        :gi [s-backspace] #'doom/backward-kill-to-bol-and-indent))



;; smartparens key-binding
(after! smartparens
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
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
             ("C-x `"  . wrap-with-back-quotes)))
