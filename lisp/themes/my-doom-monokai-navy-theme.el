;;; doom-monokai-navy-theme.el --- inspired by monokai and LY navy
(require 'doom-themes)

;;
(defgroup my-doom-monokai-navy-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom my-doom-monokai-navy-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my-doom-monokai-navy-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my-doom-monokai-navy
  "Monokai and LY navy."

  ((bg         '("#04041f" nil       nil          ))
   (bg-alt     '("#041136" nil       nil          ))
   (base0      '("#191835" "black"   "black"      ))
   (base1      '("#1B1B36" "#1B1B1B"              ))
   (base2      '("#21213E" "#212122"              ))
   (base3      '("#2B2B4C" "#2B2B2B" "brightblack"))
   (base4      '("#383550" "#3F4040" "brightblack"))
   (base5      '("#4C4A5F" "#5C5E5E" "brightblack"))
   (base6      '("#727090" "#757878" "brightblack"))
   (base7      '("#c1c0D0" "#969896" "brightblack"))
   (base8      '("#FCFCFF" "#FCFCFA" "white"      ))
   (fg         '("#FCFCFA" "#939293" "white"))
   (fg-alt     '("#939293" "#A3A2A3" "white"))

   (grey       '("#727072" "#727072" "brightblack"))
   (red        '("#CC6666" "#CC6666" "red"))
   (orange     '("#FC9867" "#FC9867" "orange"))
   (green      '("#A9DC76" "#A9DC76" "green"))
   (teal       green)
   (yellow     '("#FFD866" "#FFD866" "yellow"))
   (blue       '("#78DCE8" "#78DCE8" "blue"))
   (dark-blue  '("#81A2BE" "#81A2BE" "blue"))
   (magenta    '("#FF6188" "#FF6188" "violet"))
   (violet     '("#AB9DF2" "#AB9DF2" "violet"))
   (cyan       blue)
   (dark-cyan  dark-blue)

   ;; face categories
   (highlight      base8)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base5)
   (builtin        blue)
   (comments       grey)
   (doc-comments   yellow)
   (constants      violet)
   (functions      green)
   (keywords       magenta)
   (methods        green)
   (operators      magenta)
   (type           blue)
   (strings        yellow)
   (variables      base8)
   (numbers        violet)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when my-doom-monokai-navy-padded-modeline
      (if (integerp my-doom-monokai-navy-padded-modeline) my-doom-monokai-navy-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    `(,(car bg-alt) ,@(cdr base0)))
   (modeline-bg-l
    `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1)))
   (-modeline-pad
    (when my-doom-monokai-navy-padded-modeline
      (if (integerp my-doom-monokai-navy-padded-modeline)
          my-doom-monokai-navy-padded-modeline
        4))))

  ;; --- extra faces ------------------------
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background highlight)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground highlight)

   ;; Doom modeline
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :foreground fg :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-project-root :foreground fg :inherit 'mode-line-emphasis)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode / web-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   (web-mode-interpolate-color1-face :foreground orange)
   (web-mode-interpolate-color2-face :forground green)
   (web-mode-interpolate-color3-face :foreground blue)

   ;; markdown-mode
   (markdown-header-delimiter-face :foreground base7)
   (markdown-metadata-key-face     :foreground base7)
   (markdown-list-face             :foreground base7)
   (markdown-link-face             :foreground cyan)
   (markdown-url-face              :inherit 'link :foreground fg :weight 'normal)
   (markdown-italic-face           :inherit 'italic :foreground magenta)
   (markdown-bold-face             :inherit 'bold :foreground magenta)
   (markdown-markup-face           :foreground base7)
   (markdown-gfm-checkbox-face :foreground cyan)
   (markdown-code-face             :background bg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue :background bg)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background bg-alt)
   (org-block-begin-line :background base2 :foreground comments)

   ;; --- extra variables ---------------------
   (paren-face-match  :foreground yellow   :background (doom-darken bg 0.2) :weight 'ultra-bold)
   (tooltip           :background bg-alt :foreground fg)
   (whitespace-indentation :foreground cyan  :underline t)
   (whitespace-trailing :background orange)
   (rainbow-delimiters-depth-1-face :foreground "#3d59a1")
   (rainbow-delimiters-depth-2-face :foreground "#6183bb")
   (rainbow-delimiters-depth-3-face :foreground "#6d91de")
   (rainbow-delimiters-depth-4-face :foreground "#868bc4")
   (rainbow-delimiters-depth-5-face :foreground "#7aa2f7")
   (rainbow-delimiters-depth-6-face :foreground "#9cacff")
   (rainbow-delimiters-depth-7-face :foreground "#c0cefc")

   (centaur-tabs-unselected :foreground fg-alt :background bg)
   (centaur-tabs-unselected-modified :foreground fg-alt :background bg)

   (corfu-default :background bg-alt :foreground fg)
   (corfu-current :background dark-cyan :foreground fg)
   )
  )

;;; my-doom-monokai-navy-theme.el ends here
