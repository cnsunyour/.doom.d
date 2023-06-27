;;; cnsunyour/tabnine/config.el -*- lexical-binding: t; -*-


(use-package! company-tabnine
  :when (modulep! :completion company)
  :init
  (let ((backends (if (modulep! :editor snippets)
                      '(:separate company-tabnine company-capf company-yasnippet)
                    '(:separate company-tabnine company-capf))))
    (when (modulep! :tools lsp)
      (setq +lsp-company-backends backends))
    (set-company-backend! 'prog-mode backends)))
