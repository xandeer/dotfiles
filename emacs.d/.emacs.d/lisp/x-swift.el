;;; x-swift.el --- swift -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use 'swift-mode)
(x/package-use 'lsp-sourcekit)

(with-eval-after-load 'swift-mode
  (add-hook 'swift-mode-hook #'lsp)

  (defun find-sourcekit-lsp ()
    (or (executable-find "sourcekit-lsp")
        (and (eq system-type 'darwin)
             (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
        "/usr/local/swift/usr/bin/sourcekit-lsp"))

  (require 'lsp-sourcekit)
  (setq lsp-sourcekit-executable (find-sourcekit-lsp)))

(with-eval-after-load 'swift-mode
  (defun format-swift-buffer ()
    "Format the current buffer using swift-format."
    (interactive)
    (when (eq major-mode 'swift-mode)
      (x/start-process
       (format "swift-format -i %s" (buffer-file-name))
       nil
       (lambda (proc event)
         (when (eq (process-status proc) 'exit)
           (save-excursion (revert-buffer t t)))))))

  (define-key swift-mode-map (kbd "C-c f") #'format-swift-buffer))


(with-eval-after-load 'swift-mode
  (defun xcode-build ()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))

  (defun xcode-run ()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))

  (defun xcode-test ()
    (interactive)
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))

  (transient-define-prefix x/transient-swift ()
    "Transient for Swift."
    [["Swift"
      ("r" "Run" xcode-run)
      ("o" "Open in Xcode" (lambda () (interactive) (shell-command (concat "open " buffer-file-name))))]])

  (define-key swift-mode-map (kbd "H-k") #'x/transient-swift))

(provide 'x-swift)
;;; x-swift.el ends here
