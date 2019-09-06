;;; ~/.doom.d/+org2blog.el -*- lexical-binding: t; -*-

(use-package! ox-hugo
  :after ox
  :config
  (setq org-hugo-section "post"))

(use-package! org2blog
  :defer t
  :config
  ;; org2blog相关设置
  (setq org2blog/wp-default-title nil)
  (setq org2blog/wp-default-categories (list "个人" "技术" "家庭" "生活"))

  (let (credentials)
    ;; only required if your auth file is not already in the list of auth-sources
    ;; (add-to-list 'auth-sources "~/.authinfo")
    (setq credentials (auth-source-user-and-password "myblog"))
    (setq org2blog/wp-blog-alist
          `(("myblog"
             :url "http://www.sunyour.org/blog/xmlrpc.php"
             :username ,(car credentials)
             :password ,(cadr credentials)
             :tags-as-categories nil))))

  (setq org2blog/wp-buffer-template
        (concat "#+LATEX_HEADER: \\usepackage[UTF8]{ctex}\n"
                "# #+LATEX_HEADER: \\usepackage{CJK}\n"
                "# #+LATEX_HEADER: \\setmainfont{Hack Nerd Font}\n"
                "# #+LATEX_HEADER: \\setsansfont{Hack Nerd Font}\n"
                "# #+LATEX_HEADER: \\setmonofont{Hack Nerd Font Mono}\n"
                "# #+LATEX_HEADER: \\setCJKmainfont{Source Han Sans SC}\n"
                "# #+LATEX_HEADER: \\setCJKsansfont{Source Han Sans SC}\n"
                "# #+LATEX_HEADER: \\setCJKmonofont{WenQuanYi Micro Hei Mono}\n"
                "#+TITLE: %s\n"
                "#+AUTHOR: %s\n"
                "#+DATE: %s\n"
                "#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil\n"
                "#+CATEGORY: %s\n"
                "#+TAGS: \n"
                "#+DESCRIPTION: \n"))
  (defun org2blog/wp-format-buffer-with-author (buffer-template)
    "Default buffer formatting function."
    (format buffer-template
            ;; TITLE
            (or (plist-get (cdr org2blog/wp-blog) :default-title)
                org2blog/wp-default-title
                (read-string "请输入POST标题:"))
            ;; AUTHOR
            "不一般的凡"
            ;; DATE
            (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
            ;; CATEGORY
            (mapconcat
             (lambda (cat) cat)
             (or (plist-get (cdr org2blog/wp-blog) :default-categories)
                 org2blog/wp-default-categories)
             ", ")
            ))
  (setq org2blog/wp-buffer-format-function 'org2blog/wp-format-buffer-with-author)
  (setq org2blog/wp-show-post-in-browser 'show))
