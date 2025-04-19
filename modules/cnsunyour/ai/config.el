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

  (set-popup-rule! (regexp-quote "*SiliconFlow*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "SiliconFlow"
    :host "api.siliconflow.cn"
    :key #'gptel-api-key
    :models '(deepseek-ai/DeepSeek-V3
              deepseek-ai/DeepSeek-R1
              Qwen/Qwen2.5-32B-Instruct
              Qwen/Qwen2.5-72B-Instruct)
    :stream t)

  (set-popup-rule! (regexp-quote "*GitHub*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "GitHub"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :key #'gptel-api-key
    :models '(gpt-4o
              gpt-4.1
              DeepSeek-V3-0324
              DeepSeek-R1)
    :stream t)

  (set-popup-rule! (regexp-quote "*ZaiWen*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "ZaiWen"
    :host "autobak.zaiwen.top"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(deepseek-chat
              deepseek-reasoner
              gpt-4o
              gpt-o3-mini
              claude-3-7-sonnet-20250219
              claude-3-7-sonnet-thinking-all
              claude-3-opus-20240229
              gemini-2.0-flash-exp
              gemini-1.5-pro
              grok-beta)
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
              o3-mini)
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
    :models '(claude-3-7-sonnet-20250219-thinking
              claude-3-7-sonnet-20250219
              claude-3-opus-20240229)
    :stream t)

  (set-popup-rule! (regexp-quote "*OpenRouter*")
    :side 'left :size 100 :select t :quit 'current)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :key #'gptel-api-key
    :models '(deepseek/deepseek-chat-v3-0324
              deepseek/deepseek-r1
              openai/chatgpt-4o-latest
              openai/gpt-4.1
              openai/gpt-4.5-preview
              openai/o1
              openai/o3-mini
              openai/o3-mini-high
              openai/o3
              openai/o4-mini
              openai/o4-mini-high
              openai/o1-pro
              anthropic/claude-3.7-sonnet
              anthropic/claude-3.7-sonnet:thinking
              anthropic/claude-3-opus
              x-ai/grok-3-beta
              google/gemini-2.0-flash-001
              google/gemini-2.0-flash-thinking-exp:free
              google/gemini-2.5-pro-preview-03-25)
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
                                      :chat-model "anthropic/claude-3.7-sonnet"))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

;; we recommend using use-package to organize your init.el
(use-package! codeium
    :init
    ;; use globally
    ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    (add-hook! 'prog-mode-hook
               (pushnew! completion-at-point-functions #'codeium-completion-at-point))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    ;; (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

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
                              "openrouter/openai/o1"
                              "openrouter/anthropic/claude-3.7-sonnet"
                              "openrouter/anthropic/claude-3-opus"
                              "openrouter/x-ai/grok-3-beta"
                              "openrouter/google/gemini-2.0-flash-001"
                              "openrouter/google/gemini-pro-1.5"))
  (aidermacs-args '("--model" "openrouter/anthropic/claude-3.7-sonnet")))
