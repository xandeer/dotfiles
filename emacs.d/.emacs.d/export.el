;;; export.el --- Xandeer's Emacs configuration export script.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Xandeer's Emacs configuration export script.

;;; Code:

(require 'ox-publish)

(defvar xandeer/base)
(defvar xandeer/publish-base)
(defvar xandeer/publishing-directory)

(setq xandeer/base "~/projects/personal/dotfiles/emacs.d/.emacs.d/"
      xandeer/publish-base (expand-file-name "literate/" xandeer/base)
      xandeer/publishing-directory (expand-file-name "config/" xandeer/base)
      org-publish-timestamp-directory
      (expand-file-name "var/org/timestamps/" xandeer/base))

(setq org-publish-project-alist
      `(("init"
         :base-directory ,(expand-file-name xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name xandeer/base)
         :publishing-function org-babel-tangle-publish)
        ("bootstrap"
         :base-directory ,(expand-file-name "bootstrap/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("core"
         :base-directory ,(expand-file-name "core/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "core" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("editor"
         :base-directory ,(expand-file-name "editor/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "editor" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("tools"
         :base-directory ,(expand-file-name "tools/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "tools" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("langs"
         :base-directory ,(expand-file-name "langs/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "langs" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("org"
         :base-directory ,(expand-file-name "org/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "org" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("ui"
         :base-directory ,(expand-file-name "ui/" xandeer/publish-base)
         :base-extension "org"
         :recursive nil
         :publishing-directory ,(expand-file-name "ui" xandeer/publishing-directory)
         :publishing-function org-babel-tangle-publish)
        ("all"
         :components ("init" "bootstrap" "core" "editor" "tools" "langs" "org" "ui"))))

(provide 'export)
;;; export.el ends here
