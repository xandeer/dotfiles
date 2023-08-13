;;; x-start-process.el --- x-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar x/process-prefix "x/exec-"
  "Prefix of the subprocess.")

(defun x/process-buffer-get (program)
  "Return the process buffer associated with the PROGRAM."
  (format "*%s%s*" x/process-prefix program))

(defun x/start-process (cmd &optional switch?)
  "Execute CMD in a subprocess.
If SWITCH? is non-nil, switch to the process buffer."
  (let* ((program (car (split-string cmd)))
         (name (concat x/process-prefix program))
         (buffer (x/process-buffer-get program)))
    (apply #'make-process
           `(:name ,name
                   :buffer ,buffer
                   :command ,(mapcar (lambda (it)
                                       (if (s-starts-with? "~" it)
                                           (expand-file-name it)
                                         it))
                                     (split-string cmd))))
    (when switch?
      (switch-to-buffer buffer)
      (goto-char (point-max)))))

(provide 'x-start-process)
;;; x-start-process.el ends here
