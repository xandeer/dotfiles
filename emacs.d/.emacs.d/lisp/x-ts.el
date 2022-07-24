;;; x-ts.el --- typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; basic
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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

  (defhydra x/hydra-typescript (:exit t :columns 4 :idle 0.3)
    "
Typescript\n"
    ("d" x/docs-lookup "docs lookup at point")
    ("r" x/exercism-open-readme-other-window "open readme in other window")
    ("i" x/web-unskip-test "unskip test")
    ("t" yarn-test "yarn test")
    ("H-t" x/ts-to-js "ts to js")
    ("u" x/exercism-submit "submit to exercism"))

  (let ((map typescript-mode-map))
    (define-key map (kbd "H-k") #'x/hydra-typescript/body)))

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
        (kill-process process))))

  (defun x/ts--repl-setup ()
    "Setup ts-repl."
    (let ((map typescript-mode-hook))
      (define-key (kbd "C-x C-e") #'ts-send-last-sexp)
      (define-key (kbd "C-c C-b") #'ts-send-buffer)
      (define-key (kbd "C-c C-z") #'run-ts)
      (define-key (kbd "C-c C-k") #'x/web-kill-ts-repl)))

  (add-hook 'typescript-mode-hook #'x/ts--repl-setup))

;;; org babel
(with-eval-after-load 'org
  (defalias 'ts-mode #'typescript-mode)
  (defalias 'org-babel-execute:ts #'org-babel-execute:typescript)
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(provide 'x-ts)
;;; x-ts.el ends here
