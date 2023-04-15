;;; x-kotlin.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'kotlin-mode-hook #'flycheck-kotlin-setup)

(autoload #'org-babel-execute:kotlin "ob-kotlin" nil t)
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
               '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts
               '("kotlin" . "kt")))

(with-eval-after-load 'kotlin-mode
  (defun x/kotlin-locate-gradlew-file (&optional _dir)
    "Gradlew file location for this project."
    (when buffer-file-name
      (locate-dominating-file buffer-file-name "gradlew")))

  (defun x/kotlin-exercism-clear ()
    "Delete useless comments and reindent."
    (interactive)
    (x/replace ".*TODO.*" "")
    (x/replace "@Ignore\n" "")
    (mark-whole-buffer)
    (indent-for-tab-command))

  (defun x/kotlin-gradle-test ()
    (interactive)
    (let ((root (x/kotlin-locate-gradlew-file)))
      (async-shell-command (concat "cd " root
                                   ";gradle test")
                           "*gradle test*")))

  (defun x/kotlin-new-lib-project ()
    "Create a new kotlin project in ~/temp."
    (interactive)
    (let* ((root-name (read-from-minibuffer "Project root name: "))
           (root (expand-file-name root-name "~/temp")))
      (mkdir root)
      (shell-command (concat
                      "cd " root
                      ";gradle init --type kotlin-library --dsl kotlin"))
      (dired root)))

  (transient-define-prefix x/transient-kotlin ()
    "Transient for Kotlin."
    [["Kotlin"
      ("d" "Docs lookup at point" x/docs-lookup)
      ("r" "Open README in other window" x/exercism-open-readme-other-window)
      ("i" "Clear useless comments" x/kotlin-exercism-clear)
      ("t" "Run Gradle test" x/kotlin-gradle-test)
      ("u" "Submit to Exercism" x/exercism-submit)
      ("o" "Open in Android Studio" (lambda () (interactive) (shell-command (concat "studio " buffer-file-name))))
      ("n" "New Kotlin library project in ~/temp" x/kotlin-new-lib-project)]])

  (define-key kotlin-mode-map (kbd "H-k") #'x/transient-kotlin)

  (defun x/kotlin--setup ()
    ;; org src block
    (unless (eq major-mode 'org-mode)
      (setq-local projectile-project-root-functions (cons 'x/kotlin-locate-gradlew-file projectile-project-root-functions))))

  (add-hook 'kotlin-mode-hook #'x/kotlin--setup)
  (add-hook 'markdown-mode-hook #'x/kotlin--setup))

(provide 'x-kotlin)
;;; x-kotlin.el ends here
