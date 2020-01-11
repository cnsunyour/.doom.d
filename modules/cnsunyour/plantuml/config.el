;;; cnsunyour/plantuml/config.el -*- lexical-binding: t; -*-


;; plantuml-mode & ob-plantuml
(after! plantuml-mode
  ;; Change plantuml exec mode to `executable', other mode failed.
  (setq plantuml-default-exec-mode 'executable)
  ;; 设定plantuml的jar文件路径
  (setq plantuml-jar-path
        (cond
         (IS-MAC
          "/usr/local/opt/plantuml/libexec/plantuml.jar")
         (IS-LINUX
          "/usr/share/java/plantuml/plantuml.jar"))
        org-plantuml-jar-path plantuml-jar-path))
