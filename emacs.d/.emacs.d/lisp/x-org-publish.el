;;; x-org-publish.el --- x-org-publish -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'ox-publish)
(require 'ox-html)

;;; Path

(require 'x-utils)
(defvar x/publish-pub-dir (x/expand-repo "xandeer.github.io/"))

(defun x/expand-pub (path)
  "Expand file PATH under `x/publish-pub-dir`."
  (expand-file-name path x/publish-pub-dir))

(require 'no-littering)
(setq org-publish-timestamp-directory (no-littering-expand-var-file-name "org-pub-timestamps/"))
(setq org-html-html5-fancy                t)
(setq org-html-inline-images              t)
(setq org-export-headline-levels          6)
(setq org-html-head-include-default-style nil)

;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+intl/chinese/packages.el
(defadvice org-html-paragraph (before org-html-paragraph-advice
                                      (paragraph contents info) activate)
"Join consecutive Chinese lines into a single long line without
unwanted space when exporting `org-mode` to html."
  (let* ((origin-contents (ad-get-arg 1))
         (fix-regexp "[[:multibyte:]]")
         (fixed-contents
          (replace-regexp-in-string
           (concat
            "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
    (ad-set-arg 1 fixed-contents)))

;;; Project alist

(setq org-publish-project-alist
      `(("xandeer-org"
         :base-directory ,org-directory
         :publishing-directory ,x/publish-pub-dir
         :recursive t
         :exclude "area/\\|journal-?.*/\\|gtd/"
         :base-extension "org"
         :publishing-function org-html-publish-to-html
         :style-include-default nil
         :html-head ,(concat
                      "<link rel=\"stylesheet\" href=\"static/x.css\" type=\"text/css\"/>"
                      "<meta charset=\"utf-8\">"
                      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
         :html-postamble ,(concat
                           "<div class=\"links\">"
                           "<a href=\"#content\">T</a>"
                           "<a href=\"index.html\">H</a>"
                           "<a id=\"share-img\">I</a>"
                           "<a href=\"20210629191000-000_index.html\">0</a>"
                           "</div>"
                           "<script src=\"static/dom2img.min.js\" type=\"text/javascript\"></script>"
                           "<script src=\"static/x.js\" type=\"text/javascript\"></script>")
         :auto-sitemap t
         :sitemap-filename "index.org"
         :with-author nil
         :with-creator nil)
        ("xandeer-static"
         :base-directory ,org-directory
         :publishing-directory ,x/publish-pub-dir
         :exclude "area/\\|journal-?.*/\\|gtd/"
         :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|webp\\|js\\|clj\\|txt\\|m4a\\|mp3"
         :publishing-function org-publish-attachment
         :recursive t
         :with-author nil)
        ("xandeer"
         :components ("xandeer-static" "xandeer-org"))))

;;; A customized filter

(defun x/pub-exclude (files)
  "Exclude FILES which contens match \"^#\\+.* nopub\"."
  (cl-remove-if
   (lambda (f)
     (and (file-exists-p f)
          (string-match "^#\\+.*nopub" (org-file-contents f))))
   files))
(advice-add 'org-publish-get-base-files :filter-return 'x/pub-exclude)

(provide 'x-org-publish)
;;; x-org-publish.el ends here
