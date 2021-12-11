;;; x-json.el --- x-json -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'json-mode)

(require-package 'json-reformat)
(setq json-reformat:indent-width 2)
(setq json-reformat:pretty-string? t)

(require-package 'jq-format)

(provide 'x-json)
;;; x-json.el ends here
