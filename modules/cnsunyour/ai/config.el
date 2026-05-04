;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer 5
  :config
  (set-popup-rule! (regexp-quote "*DeepSeek*")
    :side 'left :size 100 :select t :quit 'current)
  (setq gptel-model 'deepseek-v4-pro
        gptel-backend (gptel-make-deepseek "DeepSeek"
                        :key #'gptel-api-key
                        :stream t))

  (set-popup-rule! (regexp-quote "*BigModel*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "BigModel"
    :host "open.bigmodel.cn"
    :endpoint "/api/coding/paas/v4/chat/completions"
    :key #'gptel-api-key
    :models '(glm-5.1)
    :stream t)

  (set-popup-rule! (regexp-quote "*DMXAPI*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "DMXAPI"
    :host "www.dmxapi.cn"
    :key #'gptel-api-key
    :models '(gpt-5.5
              claude-sonnet-4.6
              claude-opus-4.7
              gemini-3.1-pro-preview)
    :stream t)

  (set-popup-rule! (regexp-quote "*OpenRouter*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(openai/gpt-5.5
              anthropic/claude-sonnet-4.6
              anthropic/claude-opus-4.7
              google/gemini-3.1-pro-preview)
    :stream t)

  (add-hook! 'gptel-mode-hook
    (display-line-numbers-mode -1)
    (evil-change-state 'emacs))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package! gptel-extensions
  :after gptel)

(use-package! org-ai
  :after org
  :commands
  org-ai-mode
  org-ai-global-mode
  :preface
  (defun +org-ai-enable-global-mode-h ()
    (unless (bound-and-true-p org-ai-global-mode)
      (org-ai-global-mode 1))
    (remove-hook 'org-mode-hook #'+org-ai-enable-global-mode-h))
  :custom
  (org-ai-default-chat-model "deepseek-v4-pro")
  (org-ai-chat-models '("deepseek-v4-pro"))
  :hook
  (org-mode . org-ai-mode) ; enable org-ai in org-mode
  (org-mode . +org-ai-enable-global-mode-h) ; install C-c M-a bindings when first needed
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
  :init
  (require 'llm-openai)
  :config
  ;; The reason for using setq instead of customize is to avoid exposing the API key in the .custom file.
  (setq magit-gptcommit-llm-provider (make-llm-openai-compatible
                                      :url "https://ark.cn-beijing.volces.com/api/v3/"
                                      :key (auth-source-pick-first-password
                                            :host "ark.cn-beijing.volces.com"
                                            :user "apikey")
                                      :embedding-model "doubao-embedding-large-text-250515"
                                      :chat-model "doubao-seed-2-0-pro-260215"))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
