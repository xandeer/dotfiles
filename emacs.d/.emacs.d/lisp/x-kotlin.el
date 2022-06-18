;;; x-kotlin.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck-kotlin)
(require-package 'kotlin-mode)
(add-hook 'kotlin-mode-hook #'flycheck-kotlin-setup)

(require-package
 '(ob-kotlin :host github
             :repo "zweifisch/ob-kotlin"))
(autoload #'org-babel-execute:kotlin "ob-kotlin" nil t)
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
               '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts
               '("kotlin" . "kt")))

(defun x/kotlin-exercism-clear ()
  "Delete useless comments and reindent."
  (interactive)
  (x/replace ".*TODO.*" "")
  (x/replace "@Ignore\n" "")
  (mark-whole-buffer)
  (indent-for-tab-command))

(defun x/kotlin-locate-gradlew-file (&optional _dir)
  "Gradlew file location for this project."
  (when buffer-file-name
    (locate-dominating-file buffer-file-name "gradlew")))

(defun x/kotlin-setup ()
  (setq-local projectile-project-root-functions (cons 'x/kotlin-locate-gradlew-file projectile-project-root-functions)))

(defun x/kotlin-gradle-test ()
  (interactive)
  (let ((root (x/kotlin-locate-gradlew-file)))
    (async-shell-command (concat "cd " root
                                 ";gradle test")
                         "*gradle test*")))

(with-eval-after-load 'kotlin-mode
  (define-key kotlin-mode-map (kbd "C-c t i") #'x/kotlin-exercism-clear)
  (define-key kotlin-mode-map (kbd "C-c t n") #'x/kotlin-gradle-test)
  (define-key kotlin-mode-map (kbd "C-c e r") #'x/exercism-open-readme-other-window)
  (define-key kotlin-mode-map (kbd "C-c e u") #'x/exercism-submit)

  (add-hook 'kotlin-mode-hook #'x/kotlin-setup))

(defun x/kotlin-new-lib-project ()
  (interactive)
  (let* ((root-name (read-from-minibuffer "Project root name: "))
         (root (expand-file-name root-name "~/temp")))
    (mkdir root)
    (shell-command (concat
                    "cd " root
                    ";gradle init --type kotlin-library --dsl kotlin"))
    (dired root)))

(provide 'x-kotlin)
;;; x-kotlin.el ends here
