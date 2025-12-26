;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer 5
  :config
  (set-popup-rule! (regexp-quote "*DeepSeek*")
    :side 'left :size 100 :select t :quit 'current)
  (setq gptel-model 'deepseek-reasoner
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :key #'gptel-api-key
                        :stream t))

  ;; remove default ChatGPT provider from backends
  (dolist (item gptel--known-backends)
    (if (string= (car item) "ChatGPT")
        (setq gptel--known-backends (cl-remove item gptel--known-backends))))

  (set-popup-rule! (regexp-quote "*BigModel*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "BigModel"
    :host "open.bigmodel.cn"
    :endpoint "/api/coding/paas/v4/chat/completions"
    :key #'gptel-api-key
    :models '(GLM-4.7)
    :stream t)

  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "ChatGPT"
    :host "api.oaipro.com"
    :key #'gptel-api-key
    :models '(gpt-5.2)
    :stream t)

  (set-popup-rule! (regexp-quote "*Claude*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-anthropic "Claude"
    :host "api.oaipro.com"
    :key #'gptel-api-key
    :models '(claude-haiku-4-5-20251001
              claude-sonnet-4-5-20250929
              claude-opus-4-5-20251101)
    :stream t)

  (set-popup-rule! (regexp-quote "*OpenRouter*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(openai/gpt-5.2
              openai/gpt-5.2-pro
              anthropic/claude-sonnet-4.5
              anthropic/claude-opus-4.5
              google/gemini-3-pro-preview
              x-ai/grok-4)
    :stream t)

  (add-hook! 'gptel-mode-hook
    (display-line-numbers-mode -1)
    (evil-change-state 'emacs))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package! gptel-extensions
  :demand t
  :after gptel)

(use-package! org-ai
  :demand t
  :after org
  :commands
  org-ai-mode
  org-ai-global-mode
  :custom
  (org-ai-default-chat-model "deepseek-chat")
  (org-ai-chat-models '("deepseek-chat"
                        "deepseek-reasoner"))
  :hook
  (org-mode . org-ai-mode) ; enable org-ai in org-mode
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-openai-chat-endpoint "https://api.deepseek.com/chat/completions"
        org-ai-openai-completion-endpoint "https://api.deepseek.com/chat/completions")
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(use-package! magit-gptcommit
  :after magit
  :bind
  (:map git-commit-mode-map
        ("C-c C-g" . magit-gptcommit-commit-accept))
  (:map magit-status-mode-map
        ("C-c C-g" . magit-gptcommit-generate))
  :config
  (require 'llm-openai)
  (setq magit-gptcommit-llm-provider (make-llm-openai-compatible
                                      :url "https://openrouter.ai/api/v1/"
                                      :key (auth-source-pick-first-password
                                            :host "openrouter.ai"
                                            :user "apikey")
                                      :chat-model "anthropic/claude-haiku-4.5"))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(use-package! claude-code-ide
  :bind ("C-c C-`" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools
