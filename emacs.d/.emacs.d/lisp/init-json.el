;;; init-json.el --- init-json -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf json-mode
  :straight t
  :commands (json-mode-show-path json-mode-beautify))

(leaf json-reformat
  :straight t
  :commands json-reformat-region
  :custom
  (json-reformat:indent-width . 2)
  (json-reformat:pretty-string? . t))

(leaf jq-format
  :straight t
  :after json-mode
  :commands (jq-format-json-on-save-mode
             jq-format-json-buffer jq-format-json-region
             jq-format-jsonlines-buffer jq-format-jsonlines-region))

(provide 'init-json)
;;; init-json.el ends here
