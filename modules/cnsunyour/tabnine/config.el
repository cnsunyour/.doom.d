;;; cnsunyour/tabnine/config.el -*- lexical-binding: t; -*-


(use-package! company-tabnine
  :when (and (modulep! :completion company)
             (modulep! :tools lsp))
  :init
  (setq +lsp-company-backends
        (if (modulep! :editor snippets)
            '(:separate company-tabnine company-capf company-yasnippet)
          '(:separate company-tabnine company-capf))))
