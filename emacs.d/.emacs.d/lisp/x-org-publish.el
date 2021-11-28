;;; x-org-publish.el --- x-org-publish -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'ox-publish)
(require 'ox-html)

(defvar x/publish-pub-dir (expand-file-name "../xandeer.github.io" org-directory))

(leaf ox-publish
  :custom
  (org-html-inline-images              . t)
  (org-html-head-include-default-style . nil)
  (org-html-html5-fancy                . t)
  (org-export-headline-levels          . 6)
  (org-id-locations-file-relative      . t)
  (org-id-locations-file               . `,(expand-file-name "id-locations.el" org-directory))
  (org-id-extra-files                  . `,(remove (expand-file-name "index.org" org-directory)
                                              (directory-files org-directory 'full (rx ".org" eos))))
  (org-id-link-to-org-use-id           . 'create-if-interactive-and-no-custom-id)
  (org-attach-id-dir                   . `,(expand-file-name "attach/" org-directory))
  (org-publish-timestamp-directory     . `,(expand-file-name ".timestamps/" x/publish-pub-dir))
  (org-publish-project-alist
   . `(("xandeer-org"
        :base-directory ,org-directory
        :publishing-directory ,x/publish-pub-dir
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
        :base-directory ,org-directory
        :publishing-directory ,x/publish-pub-dir
        :exclude "area/\\|journal-?.*/\\|gtd/\\|work/\\|private/\\|copy_about_doc_norang\\|hledger.*"
        :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|webp\\|js\\|m4a"
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

(provide 'x-org-publish)
;;; x-org-publish.el ends here
