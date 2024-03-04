;;; doom-tokyo-night-theme.el --- inspired by VS Code tokyo-night
(require 'doom-themes)

;;
(defgroup my-doom-tokyo-night-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom my-doom-tokyo-night-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my-doom-tokyo-night-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my-doom-tokyo-night
  "An clean 80's synthwave / outrun theme inspired by VS Code tokyo-night."

  ;; name        default   256  16
  ((bg         '("#1a1b26" nil  nil))
   (bg-alt     '("#0f0f14" nil  nil))
   (base0      '("#1f202e" nil  nil))
   (base1      '("#20222c" nil  nil))
   (base2      '("#222333" nil  nil))
   (base3      '("#2b2b3b" nil  nil))
   (base4      '("#3b3e52" nil  nil))
   (base5      '("#42465d" nil  nil))
   (base6      '("#515c7e" nil  nil))
   (base7      '("#737aa2" nil  nil))
   (base8      '("#bbc2e0" nil  nil))
   (fg-alt     '("#787c99" nil  nil))
   (fg         '("#c7ceed" nil  nil))

   (grey       base4)
   (red        '("#db4b4b" nil  "red"          ))
   (orange     '("#e0af68" nil  "brightred"    ))
   (green      '("#1abc9c" nil  "green"        ))
   (teal       '("#9ece6a" nil  "brightgreen"  ))
   (yellow     '("#ffdb69" nil  "yellow"       ))
   (blue       '("#0db9d7" nil  "brightblue"   ))
   (dark-blue  '("#6183bb" nil  "blue"         ))
   (magenta    '("#f7768e" nil  "brightmagenta"))
   (violet     '("#bb9af7" nil  "magenta"      ))
   (cyan       '("#89ddff" nil  "brightcyan"   ))
   (dark-cyan  '("#444b6a" nil  "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base3)
   (selection      dark-blue)
   (builtin        violet)
   (comments       base6)
   (doc-comments   (doom-lighten dark-cyan 0.25))
   (constants      magenta)
   (functions      orange)
   (keywords       dark-blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      "#e2e3e9")
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when my-doom-tokyo-night-padded-modeline
      (if (integerp my-doom-tokyo-night-padded-modeline) my-doom-tokyo-night-padded-modeline 4)))

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

   (goggles-removed :background (doom-darken red 0.4))
   (goggles-added :background (doom-darken green 0.4))
   )
  )

;;; my-doom-tokyo-night-theme.el ends here
