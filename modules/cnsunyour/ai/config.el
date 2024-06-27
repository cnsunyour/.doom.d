;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  (setq gptel-model "gemini-1.5-pro-latest"
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (auth-source-pick-first-password
                              :host "gemini"
                              :user "apikey")
                        :stream t))
  (set-popup-rule! (regexp-quote "*Gemini*")
    :side 'left :size 100 :select t :quit 'current)
  (add-hook! 'gptel-mode-hook
    (display-line-numbers-mode -1)
    (evil-change-state 'emacs))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package! gptel-extensions
  :demand t
  :after gptel)

(use-package! ai-blog
  :demand t
  :after gptel easy-hugo
  :config
  (setq ai-blog-dall-e-api-key
        (auth-source-pick-first-password :host "api.openai.com")
        ai-blog-pexels-api-key
        (auth-source-pick-first-password :host "pexels-api")
        ai-blog-google-api-key
        (auth-source-pick-first-password :host "google-api(serpapi)")
        ai-blog-bing-api-key
        (auth-source-pick-first-password :host "bing-api")))

(use-package! org-ai
  :demand t
  :after org
  :commands
  org-ai-mode
  org-ai-global-mode
  :hook
  (org-mode . org-ai-mode) ; enable org-ai in org-mode
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(use-package! magit-gptcommit
  :after magit
  :bind
  (:map git-commit-mode-map
        ("C-c C-g" . magit-gptcommit-commit-accept))
  (:map magit-status-mode-map
        ("C-c C-g" . magit-gptcommit-generate))
  :config
  (setq llm-warn-on-nonfree nil
        magit-gptcommit-llm-provider (progn
                                       (require 'llm-gemini)
                                       (make-llm-gemini
                                        :key (auth-source-pick-first-password
                                              :host "generativelanguage.googleapis.com"
                                              :user "apikey")
                                        :chat-model "gemini-1.5-pro-latest")))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
