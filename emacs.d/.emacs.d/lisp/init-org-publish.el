;;; init-org-publish.el --- init-org-publish -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar xr/publish-base-dir "~/projects/personal/notes")
(defvar xr/publish-pub-dir "~/projects/personal/xandeer.github.io")
(leaf org
  :custom
  (org-html-inline-images              . t)
  (org-html-head-include-default-style . nil)
  (org-html-html5-fancy                . t)
  (org-export-headline-levels          . 6)
  (org-publish-timestamp-directory     . `,(no-littering-expand-var-file-name "org/timestamps/"))
  (org-publish-project-alist
   . `(("xandeer-org"
        :base-directory ,xr/publish-base-dir
        :publishing-directory ,xr/publish-pub-dir
        :recursive t
        :exclude "area/\\|journal-?.*/\\|gtd/\\|work/\\|copy_about_doc_norang\\|hledger.*"
        :base-extension "org"
        :publishing-function org-html-publish-to-html
        :style-include-default nil
        :html-head ,(concat
                     "<link rel=\"stylesheet\" href=\"static/x.css\" type=\"text/css\"/>"
                     "<meta charset=\"utf-8\">"
                     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
        :html-postamble ,(concat
                          "<div class=\"links\">"
                          "<a href=\"index.html\">Home</a>"
                          "<a href=\"20210629191000-000_index.html\">000</a>"
                          "</div>"
                          "<script src=\"static/x.js\" type=\"text/javascript\"></script>")
        :auto-sitemap t
        :sitemap-filename "index.org"
        :with-author nil
        :with-creator nil)
       ("xandeer-static"
        :base-directory ,xr/publish-base-dir
        :publishing-directory ,xr/publish-pub-dir
        :exclude "area/\\|journal-?.*/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang\\|hledger.*"
        :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|webp\\|js"
        :publishing-function org-publish-attachment
        :recursive t
        :with-author nil)
       ("xandeer"
        :components ("xandeer-static" "xandeer-org"))))
  :config
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+intl/chinese/packages.el
  (defun chinese/post-init-org ()
    (defadvice org-html-paragraph (before org-html-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
      (let* ((origin-contents (ad-get-arg 1))
             (fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat
                "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
        (ad-set-arg 1 fixed-contents))))
  (chinese/post-init-org))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
(org-link-types)
