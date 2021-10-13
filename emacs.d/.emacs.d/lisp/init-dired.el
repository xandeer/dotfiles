;;; init-dired.el --- init-dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf dired
  :require t
  :bind
  ("H-k" . hydra-dired/body)
  (:dired-mode-map
   ("* n" . dired-next-marked-file)
   ("* p" . dired-prev-marked-file)
   ("d"   . dired-do-delete)
   ("@"   . xr/change-hs-on-dired))
  :custom
  (insert-directory-program      . `,(executable-find "ls"))
  ;; this doesn't work
  (dired-no-confirm              . t)
  (dired-listing-switches        . "-alh")
  (dired-dwim-target             . t)
  (delete-by-moving-to-trash     . t)
  (dired-recursive-deletes       . 'always)
  (dired-recursive-copies        . 'always)
  (dired-create-destination-dirs . 'always)
  :config
  (setq dired-deletion-confirmer '(lambda (x) t))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
    _d_ownloads    _t_elega documents    _s_creenshot
    _w_ork temp    _n_otes               _e_macs.d
    _h_ome
"
   ("h" (dired "~"))
   ("d" (dired "~/Downloads"))
   ("e" (dired "~/.emacs.d"))
   ("t" (dired "~/Downloads/telega/documents"))
   ("s" (dired "~/temp/screenshot"))
   ("w" (dired "~/temp/donut"))
   ("n" (dired org-directory))
   ))

(leaf dired-x
  :require t
  :custom
  (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|.DS_Store")
  (dired-clean-confirm-killing-deleted-buffers . nil)
  :config
  (setq-default dired-omit-extensions (remove ".bin" dired-omit-extensions))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

(leaf dired-hacks
  :straight t)

(leaf dired-collapse
  :hook (dired-mode-hook . dired-collapse-mode))

(leaf dired-filter
  :hook (dired-mode-hook . dired-filter-group-mode)
  :bind
  (:dired-filter-map
   ("p" . dired-filter-pop-all))
  :custom
  (dired-filter-revert . 'always)
  (dired-filter-group-saved-groups
   . '(("default"
        ("Git"
         (directory . ".git")
         (file . ".gitignore"))
        ("Directory"
         (directory))
        ("PDF"
         (extension . "pdf"))
        ("LaTeX"
         (extension "tex" "bib"))
        ("Source"
         (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
        ("Doc"
         (extension "md" "rst" "txt"))
        ("Org"
         (extension . "org"))
        ("Archives"
         (extension "zip" "rar" "gz" "bz2" "tar"))
        ("Images"
         (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF"))))))

(leaf dired-rainbow
  :require t
  :config
  (dired-rainbow-define html "#eb5286"
                        ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024"
                        ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2"
                        ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#8aadff"
                        ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd"
                        ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f"
                        ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b"
                        ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11"
                        ("log"))
  (dired-rainbow-define shell "#f6993f"
                        ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172"
                        ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5"
                        ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff"
                        ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a"
                        ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63"
                        ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a"
                        ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb"
                        ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f"
                        ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9"
                        ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(leaf dired-narrow
  :bind
  (:dired-narrow-map
   ("<down>"  . dired-narrow-next-file)
   ("<up>"    . dired-narrow-previous-file)
   ("<right>" . dired-narrow-enter-directory)))

(provide 'init-dired)
;;; init-dired.el ends here
