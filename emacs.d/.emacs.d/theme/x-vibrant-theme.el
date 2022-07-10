;; x-vibrant-theme.el --- a more vibrant version of doom-one -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;;
(defgroup x/vibrant-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom x/vibrant-brighter-modeline t
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'x/vibrant-theme
  :type 'boolean)

(defcustom x/vibrant-brighter-comments t
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'x/vibrant-theme
  :type 'boolean)

(defcustom x/vibrant-comment-bg x/vibrant-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'x/vibrant-theme
  :type 'boolean)

(defcustom x/vibrant-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'x/vibrant-theme
  :type '(choice integer boolean))


;;
(def-doom-theme x-vibrant
  "A dark theme based off of doom-one with more vibrant colors."

  ;; name        gui       256       16
  ((bg         '("#242730" nil       nil))
   (bg-alt     '("#2a2e38" nil       nil))
   (base0      '("#1c1f24" "#101010" "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#21272d" "#21212d" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#484854" "#5e5e5e" "brightblack"  ))
   (base5      '("#62686E" "#666666" "brightblack"  ))
   (base6      '("#757B80" "#7b7b7b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#bbc2cf" "#bfbfbf" ))
   (fg-alt     '("#5D656B" "#5d5d5d" ))

   (grey       base4)
   (red        '("#ff665c" "#ff6655" ))
   (orange     '("#e69055" "#dd8844" ))
   (green      '("#7bc275" "#99bb66" ))
   (teal       '("#4db5bd" "#44b9b1" ))
   (yellow     '("#FCCE7B"           ))
   (blue       '("#51afef"           ))
   (dark-blue  '("#1f5582"           ))
   (magenta    '("#C57BDB"           ))
   (violet     '("#a991f1"           )) ;a9a1e1
   (cyan       '("#5cEfFF"           ))
   (dark-cyan  '("#6A8FBF"           ))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if x/vibrant-brighter-comments dark-cyan base5))
   (doc-comments   (if x/vibrant-brighter-comments (doom-lighten dark-cyan 0.15) (doom-lighten base4 0.3)))
   (constants      violet)
   (functions      cyan)
   (keywords       blue)
   (methods        violet)
   (operators      magenta)
   (type           yellow)
   (strings        green)
   (variables      base8)
   (numbers        orange)
   (region         "#3d4451")
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   (level1 blue)
   (level2 magenta)
   (level3 violet)
   (level4 dark-cyan)
   (level5 orange)
   (level6 dark-blue)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when x/vibrant-padded-modeline
      (if (integerp x/vibrant-padded-modeline) x/vibrant-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey (if x/vibrant-brighter-modeline 0.4 0.08)))

   (modeline-bg
    (if x/vibrant-brighter-modeline
        `("#383f58" ,@(cdr base1))
      `(,(car bg-alt) ,@(cdr base0))))
   (modeline-bg-l
    (if x/vibrant-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.25))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))


  ;; --- extra faces ------------------------
  (((all-the-icons-dblue &override) :foreground dark-cyan)
   (centaur-tabs-unselected :background bg-alt :foreground base6)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (font-lock-comment-face
    :foreground comments
    :background (if x/vibrant-comment-bg (doom-darken bg-alt 0.095)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)

   (doom-modeline-bar :background (if x/vibrant-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if x/vibrant-brighter-modeline base8 blue) :bold bold)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if x/vibrant-brighter-modeline base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (whitespace-empty :background base2)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)

   ;; org-mode
   (org-hide :foreground hidden)

   ((outline-1 &override) :foreground level1)
   (outline-2 :inherit 'outline-1 :foreground level2)
   (outline-3 :inherit 'outline-1 :foreground level3)
   (outline-4 :inherit 'outline-1 :foreground level4)
   (outline-5 :inherit 'outline-1 :foreground level5)
   (outline-6 :inherit 'outline-1 :foreground level6)

   (org-document-title :inherit 'bold :foreground orange)
   ; (org-todo :foreground orange :bold 'inherit :background (doom-darken base1 0.02))
   ; (org-done :foreground green :strike-through nil :background base2 :bold t)
   ; (org-headline-done :foreground base4 :strike-through nil)
   ; ((org-tag &override) :foreground (doom-lighten orange 0.3))
   ; (org-agenda-date :foreground cyan)
   ; (org-agenda-dimmed-todo-face :foreground comments)
   ; (org-agenda-done :foreground base4)
   ; (org-agenda-structure :foreground violet)
   ; ((org-block &override) :background (doom-darken base1 0.125) :foreground violet)
   ; ((org-block-begin-line &override) :background (doom-darken base1 0.125) :foreground comments)
   ; (org-code :foreground yellow)
   ; (org-column :background base1)
   ; (org-column-title :background base1 :bold t :underline t)
   (org-date :foreground base5)
   ; (org-document-info :foreground blue)
   ; (org-document-info-keyworfd :foreground comments)
   ; (org-ellipsis :foreground comments)
   ; (org-footnote :foreground blue)
   ; (org-headline-base :foreground comments :strike-through t :bold nil)
   ; (org-link :foreground blue :underline t :weight 'bold)
   ; (org-priority :foreground cyan)
   ; (org-scheduled :foreground green)
   ; (org-scheduled-previously :foreground yellow)
   ; (org-scheduled-today :foreground orange)
   ; (org-sexp-date :foreground base4)
   ; (org-special-keyword :foreground yellow)
   ; (org-table :foreground violet)
   ; (org-upcoming-deadline :foreground yellow)
   ; (org-warning :foreground magenta)

   ;; tooltip and company
   (tooltip              :background bg-alt :foreground fg)
   (company-tooltip-selection     :background base4)

   (solaire-org-hide-face :foreground hidden-alt))
  ;; --- extra variables --------------------
  ;; ()
  )

;;; x-vibrant-theme.el ends here
