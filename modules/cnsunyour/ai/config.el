;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :hook
  (gptel-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (evil-change-state 'emacs)))
  (gptel-post-response . (lambda ()
                           (pcase (buffer-local-value 'major-mode (current-buffer))
                             ('org-mode (org-next-visible-heading 1) (org-end-of-line))
                             ('markdown-mode (markdown-outline-next) (end-of-line)))))
  :config
  (setq gptel-default-mode 'markdown-mode)
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'left :size 100 :select t :quit 'current))

(use-package! gptel-extensions
  :demand t
  :after gptel)

(use-package! ai-blog
  :demand t
  :after (gptel easy-hugo)
  :config
  (setq ai-blog-dall-e-api-key
        (auth-source-pick-first-password :host "api.openai.com")
        ai-blog-pexels-api-key
        (auth-source-pick-first-password :host "pexels-api")
        ai-blog-google-api-key
        (auth-source-pick-first-password :host "google-api(serpapi)")
        ai-blog-bing-api-key
        (auth-source-pick-first-password :host "bing-api")))

(use-package org-ai
  :after (org)
  :commands (org-ai-mode
             org-ai-global-mode)
  :custom
  (org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  :hook
  (org-mode . org-ai-mode) ; enable org-ai in org-mode
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets
