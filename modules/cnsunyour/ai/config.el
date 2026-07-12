;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer 5
  :config
  (set-popup-rule! (regexp-quote "*BigModel*")
    :side 'left :size 100 :select t :quit 'current)
  (setq gptel-model 'glm-5.2
        gptel-backend (gptel-make-openai "BigModel"
                        :host "open.bigmodel.cn"
                        :endpoint "/api/coding/paas/v4/chat/completions"
                        :key #'gptel-api-key
                        :models '(glm-5.2)
                        :stream t))

  (set-popup-rule! (regexp-quote "*DeepSeek*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-deepseek "DeepSeek"
    :key #'gptel-api-key
    :stream t)
  
  (set-popup-rule! (regexp-quote "*OpenRouter*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(openrouter/fusion
              ~moonshotai/kimi-latest
              ~x-ai/grok-latest
              ~openai/gpt-mini-latest
              ~openai/gpt-latest
              ~anthropic/claude-haiku-latest
              ~anthropic/claude-sonnet-latest
              ~anthropic/claude-opus-latest
              ~anthropic/claude-fable-latest
              ~google/gemini-flash-latest
              ~google/gemini-pro-latest)
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

(use-package! gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install))
