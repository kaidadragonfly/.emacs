(deftheme terminal-colors
  "Created 2023-07-21.")

(custom-theme-set-faces
 'terminal-colors
 '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "unspecified-fg" :background "unspecified-bg" :stipple nil :inherit nil))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(highlight ((t (:background "green" :foreground "black"))))
 '(region ((t (:extend t :foreground "brightwhite" :background "yellow"))))
 '(shadow ((t (:foreground "brightblack"))))
 '(secondary-selection ((t (:extend t :background "yellow"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(font-lock-builtin-face ((t (:foreground "brightmagenta"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-constant-face ((t (:foreground "brightblue"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "magenta"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "blue"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(font-lock-warning-face ((t (:weight bold :underline (:color foreground-color :style line) :foreground "brightyellow"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "cyan" :underline t))))
 '(link-visited ((t (:inherit link :foreground "brightmagenta"))))
 '(fringe ((t (:background "white"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((t (:inherit variable-pitch :background "brightyellow" :foreground "black"))))
 '(mode-line ((t (:background "white" :foreground "black" :box (:line-width (1 . -1) :style released-button)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((supports :box t) (class color) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "white" :box (:line-width (1 . -1) :color "brightblack") :weight light))))
 '(isearch ((t (:foreground "white" :background "magenta"))))
 '(isearch-fail ((t (:foreground "white" :background "red"))))
 '(lazy-highlight ((t (:foreground "black" :background "brightcyan"))))
 '(match ((t (:foreground "black" :background "brightyellow"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'terminal-colors)