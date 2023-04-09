;;; x-typescript.el --- typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; basic
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (x/major-mode-lighter 'typescript-mode "TS")

  (defun x/web-unskip-test ()
    "Unskip test in current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "xit" nil t)
        (replace-match "it"))))

  (defun x/ts-to-js ()
    "Run `tsc -out' on current buffer."
    (interactive)
    (let* ((input (buffer-name))
           (name (file-name-sans-extension input))
           (output (make-temp-file name nil ".js")))
      (if (not input)
          (error "Buffer is not visiting a file"))
      (x/start-process (format "tsc -t es2022 -m amd --moduleResolution node --outFile %s %s" output input))
      (find-file output)))

  (defun x/ts-restart-tide-and-lsp ()
    "Restart tide and lsp."
    (interactive)
    (tide-restart-server)
    (lsp-restart-workspace))

  (define-transient-command x/transient-typescript ()
    "Transient for TypeScript."
    [["Typescript"
      ("d" "Docs lookup at point" x/docs-lookup)
      ("r" "Open readme in other window" x/exercism-open-readme-other-window)
      ("H-r" "Restart tide and lsp" x/ts-restart-tide-and-lsp)
      ("i" "Unskip test" x/web-unskip-test)
      ("t" "Yarn test" yarn-test)
      ("H-t" "TS to JS" x/ts-to-js)
      ("u" "Submit to Exercism" x/exercism-submit)]])

  (define-key typescript-mode-map (kbd "H-k") #'x/transient-typescript))

;;; repl
;; yarn add global typescript ts-node
;; for console
;; yarn add global @types/node
(setq ts-comint-program-command "ts-node")
(setq ts-comint-program-arguments '("--skipProject"))

(with-eval-after-load 'ts-comint
  ;; redefine it to disable reset `ts-comint-program-arguments'
  (defun run-ts (&optional cmd dont-switch-p)
    "Run an inferior Typescript process, via buffer `*Typescript*'.
If there is a process already running in `*Typescript*', switch
to that buffer.  With argument `CMD', allows you to edit the
command line (default is value of `ts-comint-program-command').
Runs the hook `ts-comint-mode-hook' \(after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the
process buffer for a list of commands). Use `DONT-SWITCH-P' to
prevent switching to the new buffer once created."
    (interactive
     (list
      (when current-prefix-arg
        (read-string "Run typescript: "
                     (mapconcat
                      'identity
                      (cons
                       ts-comint-program-command
                       ts-comint-program-arguments)
                      " ")))))

    ;; (when cmd
    ;; (setq ts-comint-program-arguments (split-string cmd))
    ;; (setq ts-comint-program-command (pop ts-comint-program-arguments)))

    (if (not (comint-check-proc "*Typescript*"))
        (with-current-buffer
            (apply 'make-comint "Typescript" ts-comint-program-command
                   nil ts-comint-program-arguments)
          (ts-comint-mode)))
    (setq ts-comint-buffer "*Typescript*")
    (if (not dont-switch-p)
        (pop-to-buffer "*Typescript*"))

    ;; apply terminal preferences
    (if ts-comint-mode-ansi-color
        (progn
          ;; based on
          ;; http://stackoverflow.com/questions/13862471/using-node-ts-with-ts-comint-in-emacs

          ;; We like nice colors
          (ansi-color-for-comint-mode-on)
          ;; Deal with some prompt nonsense
          (make-local-variable 'comint-preoutput-filter-functions)
          (add-to-list
           'comint-preoutput-filter-functions
           (lambda (output)
             (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output))))
      (setenv "NODE_NO_READLINE" "1")))

  (defun x/web-kill-ts-repl ()
    "Kill the typescript REPL process."
    (interactive)
    (let ((process (get-buffer-process ts-comint-buffer)))
      (when process
        (kill-process process)))))

(defun x/ts--repl-setup ()
  "Setup ts-repl."
  (let ((map typescript-mode-map))
    (define-key map (kbd "C-x C-e") #'ts-send-last-sexp)
    (define-key map (kbd "C-c C-b") #'ts-send-buffer)
    (define-key map (kbd "C-c C-z") #'run-ts)
    (define-key map (kbd "C-c C-k") #'x/web-kill-ts-repl)))

(add-hook 'typescript-mode-hook #'x/ts--repl-setup)

;;; org babel
(with-eval-after-load 'org
  (defalias 'ts-mode #'typescript-mode)
  (defalias 'org-babel-execute:ts #'org-babel-execute:typescript)
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(provide 'x-typescript)
;;; x-typescript.el ends here
