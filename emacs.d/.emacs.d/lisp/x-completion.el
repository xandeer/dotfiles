;;; x-completion.el --- x-completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; corfu
(require 'corfu)
(setq corfu-auto t)
(setq corfu-auto-delay 0.1)
(setq corfu-auto-prefix 2)
(setq corfu-max-width 60)
(setq corfu-quit-no-match t)
(setq tab-always-indent 'complete)
;; (set-face-background 'corfu-current "#e5dfb0")

(x/define-keys corfu-map
               '(("M-s"       corfu-insert-separator)
                 ("<tab>"     x/tab)
                 ("<return>"  corfu-insert)
                 ("<escape>"  corfu-quit)))
;; (define-key corfu-map " " #'corfu-insert-separator)
;; (define-key corfu-map [(shift tab)] #'corfu-previous)
;; (define-key corfu-map [return] #'corfu-insert)
;; (define-key corfu-map [escape] #'corfu-quit)
(global-corfu-mode)

;;; corfu-history
(corfu-history-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

;;; corfu-quick
(x/define-keys
 corfu-map
 '(("M-q" corfu-quick-complete)
   ("C-q" corfu-quick-insert)))

;;; icon
(require 'kind-icon)

(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;;; corfu-popupinfo
(corfu-popupinfo-mode 1)

(x/define-keys
 corfu-map
 '(("M-d" corfu-popupinfo-toggle)
   ("M-n" corfu-popupinfo-scroll-up)
   ("M-p" corfu-popupinfo-scroll-down)))

;;; cape
(setq cape-dabbrev-min-length 2)

(add-to-list 'completion-at-point-functions #'cape-file)
;; (add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-abbrev)

(autoload 'company-grab-symbol-cons "company")

(defun x-completion--disable-auto-locally ()
  "Disable auto completing locally."
  (setq-local corfu-auto nil)
  (corfu-mode))

;;; eshell
(add-hook 'eshell-mode-hook #'x-completion--disable-auto-locally)

;;; org
(with-eval-after-load 'org
  (defun x-completion--org-setup ()
    ;; (x-completion--disable-auto-locally)
    (setq-local corfu-auto-prefix 2)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-abbrev))

  (add-hook 'org-mode-hook #'x-completion--org-setup))

;;; minibuffer
(defun x-completion--enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'x-completion--enable-in-minibuffer)

;;; copilot
(add-hook 'prog-mode-hook 'copilot-mode)

(defun x/tab ()
  (interactive)
  (or (copilot-accept-completion)
      (not (= (corfu-next) -1))
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (advice-add
   'copilot--start-agent
   :override
   (lambda ()
     "Start the copilot agent process in local."
     (setq copilot--connection
           (make-instance 'jsonrpc-process-connection
                          :name "copilot"
                          :events-buffer-scrollback-size copilot-log-max
                          :process
                          (make-process
                           :name "copilot agent"
                           :command (list "nix"
                                          "run"
                                          "--offline"
                                          "--impure"
                                          "nixpkgs#nodejs-slim_16"
                                          "--"
                                          (concat copilot--base-dir "/dist/agent.js"))
                           :coding 'utf-8-emacs-unix
                           :connection-type 'pipe
                           :stderr (get-buffer-create "*copilot stderr*")
                           :noquery t)))
     (message "Copilot agent started.")
     (copilot--request 'initialize '(:capabilities '()))
     (copilot--async-request 'setEditorInfo
                             `(:editorInfo (:name "Emacs" :version ,emacs-version)
                                           :editorPluginInfo (:name "copilot.el" :version ,copilot-version)))))

  (x/define-keys
   copilot-mode-map
   '(("TAB" x/tab)
     ("C-<tab>" copilot-clear-overlay)
     ("M-<tab>" copilot-next-completion))))

(x/define-keys
 global-map
 '(("TAB" x/tab)))

(provide 'x-completion)
;;; x-completion.el ends here
