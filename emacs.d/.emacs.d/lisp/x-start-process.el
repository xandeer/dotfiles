;;; x-start-process.el --- x-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar x/process-prefix "x/exec-"
  "Prefix of the subprocess.")

(defun x/process-buffer-get (program)
  "Return the process buffer associated with the PROGRAM."
  (format "*%s%s*" x/process-prefix program))

(defun x/start-process (cmd &optional switch? sentinel)
  "Execute CMD in a subprocess.
If SWITCH? is non-nil, switch to the process buffer."
  (let* ((program (car (split-string cmd)))
         (name (concat x/process-prefix program))
         (buffer (x/process-buffer-get program))
         (proc (apply #'make-process
                      `(:name ,name
                              :buffer ,buffer
                              :command ,(mapcar (lambda (it)
                                                  (if (s-starts-with? "~" it)
                                                      (expand-file-name it)
                                                    it))
                                                (split-string cmd))))))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (message "Process %s: %s" (process-name proc) event)
       (when sentinel
         (funcall sentinel proc event))))

    (when switch?
      (switch-to-buffer buffer)
      (goto-char (point-max)))))

(provide 'x-start-process)
;;; x-start-process.el ends here
