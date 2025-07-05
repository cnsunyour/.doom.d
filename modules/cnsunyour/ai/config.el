;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
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

  (set-popup-rule! (regexp-quote "*ZaiWen*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "ZaiWen"
    :host "autobak.zaiwen.top"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(deepseek-chat
              deepseek-reasoner
              gpt-4o
              gpt_o3
              gpt4_1
              gpt4.5
              o1-all
              o1-pro
              claude-3-7-sonnet-20250219
              claude-3-7-sonnet-thinking-all
              claude-3-opus-20240229
              gemini_2_5_flash
              gemini_2_5_pro
              grok3)
    :stream t)

  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "ChatGPT"
    :host "api.oaipro.com"
    :key #'gptel-api-key
    :models '(chatgpt-4o-latest
              gpt-4.1
              gpt-4.5-preview
              o1
              o3
              o4-mini)
    :stream t)

  (set-popup-rule! (regexp-quote "*Gemini*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-gemini "Gemini"
    :key #'gptel-api-key
    :stream t)

  (set-popup-rule! (regexp-quote "*Claude*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-anthropic "Claude"
    :host "api.oaipro.com"
    :key #'gptel-api-key
    :models '(claude-sonnet-4-20250514
              claude-opus-4-20250514)
    :stream t)

  (set-popup-rule! (regexp-quote "*OpenRouter*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(deepseek/deepseek-chat-v3-0324
              deepseek/deepseek-r1-0528
              openai/chatgpt-4o-latest
              openai/gpt-4.1
              openai/gpt-4.5-preview
              openai/o3-mini
              openai/o4-mini
              openai/o1
              openai/o3
              openai/o1-pro
              openai/o3-pro
              anthropic/claude-sonnet-4
              anthropic/claude-opus-4
              x-ai/grok-3-mini
              x-ai/grok-3
              google/gemini-2.5-flash
              google/gemini-2.5-pro)
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
                                      :chat-model "anthropic/claude-sonnet-4"))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(use-package! aidermacs
  :defer t
  :config
  (setenv "DEEPSEEK_API_KEY" (auth-source-pick-first-password
                              :host "api.deepseek.com"
                              :user "apikey"))
  (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password
                                :host "openrouter.ai"
                                :user "apikey"))
  :bind
  ("C-c a" . aidermacs-transient-menu)
  :custom
  (aidermacs-popular-models '("deepseek/deepseek-chat"
                              "deepseek/deepseek-reasoner"
                              "openrouter/deepseek/deepseek-chat"
                              "openrouter/deepseek/deepseek-r1"
                              "openrouter/openai/gpt-4o"
                              "openrouter/openai/o3-mini"
                              "openrouter/openai/o1"
                              "openrouter/anthropic/claude-3.7-sonnet"
                              "openrouter/anthropic/claude-3-opus"
                              "openrouter/google/gemini-2.5-flash"
                              "openrouter/google/gemini-2.5-pro"
                              "openrouter/x-ai/grok-3-beta"))
  (aidermacs-args '("--model" "openrouter/anthropic/claude-3.7-sonnet")))
