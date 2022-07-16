;;; x-clojure.el --- x-clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx$" . clojure-mode))

(with-eval-after-load 'clojure-mode
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2))

  ;; TODO: What does this for?
  ;; (leaf clojure-mode-extra-font-locking
  ;; :after clojure-mode)

  (defun x/cider-hook ()
    (add-hook 'before-save-hook #'cider-format-buffer nil t))

  ;; https://docs.cider.mx/cider/0.23/repl/configuration.html#_set_ns_in_repl
  (setq cider-repl-require-ns-on-set t
        cider-enhanced-cljs-completion-p nil
        cider-repl-wrap-history t)

  ;; these help me out with the way I usually develop web apps
  (defun cider-start-http-server ()
    (interactive)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))

  (defun x/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (defun x/cider-node-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl)
      (goto-char (point-max))
      (insert "(do (require 'cljs.repl.node) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
      (cider-repl-return)))

  (defun x/start-cider-repl-with-profile (profile)
    (interactive "sEnter profile name: ")
    (letrec ((lein-params (concat "with-profile +" profile " repl :headless")))
      (message "lein-params set to: %s" lein-params)
      (set-variable 'cider-lein-parameters lein-params)
      (cider-jack-in '())
      (set-variable 'cider-lein-parameters "repl :headless")))

  (defun x/browse-current-ns ()
    (interactive)
    (cider-browse-ns (cider-current-ns)))

  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojurescript-mode-hook 'cider-mode))

(provide 'x-clojure)
;;; x-clojure.el ends here
