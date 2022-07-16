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

  (defun x/kotlin-open-in-studio ()
    "Open the current file in Android Studio."
    (interactive)
    (shell-command (concat "studio " buffer-file-name)))

  (defhydra x/hydra-kotlin (:exit t :columns 4 :idle 0.3)
    "
Kotlin\n"
    ("d" x/devdocs-lookup "devdocs lookup at point")
    ("r" x/exercism-open-readme-other-window "open readme in other window")
    ("i" x/kotlin-exercism-clear "clear useless comments")
    ("t" x/kotlin-gradle-test "run gradle test")
    ("u" x/exercism-submit "submit to exercism")
    ("o" x/kotlin-open-in-studio "open in Android Studio")
    ("n" x/kotlin-new-lib-project "new kotlin library project in ~/temp"))
  (define-key kotlin-mode-map (kbd "H-k") #'x/hydra-kotlin/body)

  (defun x/kotlin--setup ()
    (setq-local projectile-project-root-functions (cons 'x/kotlin-locate-gradlew-file projectile-project-root-functions)))

  (add-hook 'kotlin-mode-hook #'x/kotlin--setup)
  (add-hook 'markdown-mode-hook #'x/kotlin--setup))

(provide 'x-kotlin)
;;; x-kotlin.el ends here
