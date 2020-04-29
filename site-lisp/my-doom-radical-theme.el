;;; doom-laserwave-theme.el --- inspired by VS Code radical
(require 'doom-themes)

;;
(defgroup my/doom-radical-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom my/doom-radical-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my/doom-radical-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my/doom-radical
  "A dark theme for radical hacking inspired by retro futuristic design."

  ;; name        default   256  16
  ((bg         '("#141322" nil  nil ))
   (bg-alt     '("#1c1a30" nil  nil ))
   (base0      '("#222745" nil  nil ))
   (base1      '("#24262D" nil  nil ))
   (base2      '("#262b4b" nil  nil ))
   (base3      '("#3f405f" nil  nil ))
   (base4      '("#415e6c" nil  nil ))
   (base5      '("#48676A" nil  nil ))
   (base6      '("#508695" nil  nil ))
   (base7      '("#74A39D" nil  nil ))
   (base8      '("#85a5a0" nil  nil ))
   (fg-alt     '("#e4fdf7" nil  nil ))
   (fg         '("#c7e3ee" nil  nil ))

   (grey       base4)
   (red        '("#ff1767" nil  nil ))
   (orange     '("#ffd000" nil  nil ))
   (green      '("#d6fd6b" nil  nil ))
   (teal       '("#78efc5" nil  nil ))
   (yellow     '("#edf179" nil  nil ))
   (blue       '("#008fe9" nil  nil ))
   (dark-blue  '("#070a91" nil  nil ))
   (magenta    '("#ff428e" nil  nil ))
   (violet     '("#ff85a1" nil  nil ))
   (cyan       '("#a9fef7" nil  nil ))
   (dark-cyan  '("#c7e3ee" nil  nil ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   magenta)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       "#508695")
   (doc-comments   "#7cb3b6")
   (constants      "#d5358f")
   (functions      magenta)
   (keywords       "#fa61b8")
   (methods        dark-cyan)
   (operators      cyan)
   (type           yellow)
   (strings        cyan)
   (variables      dark-cyan)
   (numbers        orange)
   (region         base2)
   (error          "#ff1767")
   (warning        "#ffd000")
   (success        "#93E0E3")
   (vc-modified    "#ffb000")
   (vc-added       "#a3ff57")
   (vc-deleted     "#ff427b")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when my/doom-radical-padded-modeline
      (if (integerp my/doom-radical-padded-modeline) my/doom-radical-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    `(,(car bg-alt) ,@(cdr base0)))
   (modeline-bg-l
    `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))

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
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

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
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;; --- extra variables ---------------------
   (paren-face-match  :foreground (doom-lighten yellow 0.5)  :background (doom-darken bg 0.2) :weight 'ultra-bold)
   (ivy-current-match :background base3 :distant-foreground nil)
   (tooltip           :background bg-alt :foreground fg)
   (company-box-background :foreground fg :background bg-alt)
   (whitespace-indentation :foreground cyan  :underline t)
   (whitespace-trailing :background orange)
   (indent-guide-face :foreground base3)
   (rainbow-delimiters-depth-1-face :foreground "#eefc53")
   (rainbow-delimiters-depth-2-face :foreground "#f0dd67")
   (rainbow-delimiters-depth-3-face :foreground "#f3be7b")
   (rainbow-delimiters-depth-4-face :foreground "#f59f90")
   (rainbow-delimiters-depth-5-face :foreground "#f880a4")
   (rainbow-delimiters-depth-6-face :foreground "#fa61b8")
   (rainbow-delimiters-depth-7-face :foreground "#fd43cd")))

;;; my/doom-radical-theme.el ends here
