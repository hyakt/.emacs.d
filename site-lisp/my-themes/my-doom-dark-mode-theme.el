;;; doom-laserwave-theme.el --- inspired by VS Code radical
(require 'doom-themes)

;;
(defgroup my/doom-dark-mode-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom my/doom-dark-mode-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my/doom-dark-mode-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my/doom-dark-mode
  "A dark theme for radical hacking inspired by retro futuristic design."

  ;; name        default   256  16
   ((bg        '("#1a1a20" nil  nil ))
   (bg-alt     '("#1a1a20" nil  nil ))
   (base0      '("#292a30" nil  nil ))
   (base1      '("#2c2d2f" nil  nil ))
   (base2      '("#373737" nil  nil ))
   (base3      '("#414141" nil  nil ))
   (base4      '("#515151" nil  nil ))
   (base5      '("#616161" nil  nil ))
   (base6      '("#7f8c98" nil  nil ))
   (base7      '("#949596" nil  nil ))
   (base8      '("#d9d9d9" nil  nil ))
   (fg-alt     '("#e1efff" nil  nil ))
   (fg         '("#dedcde" nil  nil ))

   (grey       base4)
   (red        '("#ff628c" nil  nil ))
   (orange     '("#ff8170" nil  nil ))
   (green      '("#7ec16e" nil  nil ))
   (teal       '("#a5ff90" nil  nil ))
   (yellow     '("#ffc600" nil  nil ))
   (blue       '("#157dcd" nil  nil ))
   (dark-blue  '("#0369d9" nil  nil ))
   (magenta    '("#f977ae" nil  nil ))
   (violet     '("#a79df7" nil  nil ))
   (cyan       '("#7ae3f7" nil  nil ))
   (dark-cyan  '("#80fcff" nil  nil ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base3)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       "#7f8c98")
   (doc-comments   "#7cb3b6")
   (constants      magenta)
   (functions      cyan)
   (keywords       magenta)
   (methods        dark-cyan)

   (operators      fg)
   (type           yellow)
   (strings        orange)
   (variables      fg)
   (numbers        orange)
   (region         base2)
   (error          "#F92672")
   (warning        "#ffd000")
   (success        "#93E0E3")
   (vc-modified    "#ffb000")
   (vc-added       "#a3ff57")
   (vc-deleted     "#F92672")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when my/doom-dark-mode-padded-modeline
      (if (integerp my/doom-dark-mode-padded-modeline) my/doom-dark-mode-padded-modeline 4)))

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

   (doom-modeline-bar :background base1)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground highlight)

   ;; Doom modeline
   (doom-modeline-buffer-file :foreground fg :weight 'bold)
   (doom-modeline-buffer-path :foreground fg :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-project-root :foreground fg)

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
   (rainbow-delimiters-depth-1-face :foreground base8)
   (rainbow-delimiters-depth-2-face :foreground base7)
   (rainbow-delimiters-depth-3-face :foreground base6)
   (rainbow-delimiters-depth-4-face :foreground base5)
   (rainbow-delimiters-depth-5-face :foreground base4)
   (rainbow-delimiters-depth-6-face :foreground base3)
   (rainbow-delimiters-depth-7-face :foreground base2)))

;;; my/doom-dark-mode-theme.el ends here
