;;; cnsunyour/tabnine/config.el -*- lexical-binding: t; -*-


;; tabnine，一个非常牛的补全插件
(use-package! company-tabnine
  :when (featurep! :completion company)
  :hook
  ('kill-emacs . company-tabnine-kill-process)
  ('lsp-after-open . (lambda ()
                       (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                       (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
  ;; ('after-init . (lambda ()
  ;;                 ;; (add-to-list 'company-backends #'company-tabnine)
  ;;                 ;; (set-company-backend! 'text-mode
  ;;                 ;;   'company-tabnine 'company-dabbrev 'company-yasnippet 'company-ispell)
  ;;                 (set-company-backend! 'conf-mode
  ;;                   'company-tabnine 'company-capf 'company-dabbrev-code 'company-yasnippet)
  ;;                 (set-company-backend! 'prog-mode
  ;;                   'company-tabnine 'company-capf 'company-yasnippet)))
  :config
  (map! (:leader
          :desc "Use company default backend" "clo" #'company-other-backend
          :desc "Use company tabnine backend" "clt" #'company-tabnine))
  :init
  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6))))))
