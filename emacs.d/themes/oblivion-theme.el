(deftheme oblivion "Dark theme inspired to Gedit's oblivion theme")

(let ((class '((class color) (min-colors 89)))
  ;; color definitions
  (butter1     "#fce94f")
  (butter2     "#edd400")
  (butter3     "#c4a000")
  (chameleon1  "#8ae234")
  (chameleon2  "#73d216")
  (chameleon3  "#4e9a06")
  (turquoise   "#00ffff")
  (orange1     "#fcaf3e")
  (orange2     "#f57900")
  (orange3     "#ce5c00")
  (skyblue1    "#729fcf")
  (skyblue2    "#3465a4")
  (skyblue3    "#204a87")
  (plum1       "#ad7fa8")
  (plum2       "#75507b")
  (plum3       "#5c3566")
  (chocolate1  "#e9b96e")
  (chocolate2  "#c17d11")
  (chocolate3  "#8f5902")
  (scarletred1 "#ef2929")
  (scarletred2 "#cc0000")
  (scarletred3 "#a40000")
  (aluminium1  "#eeeeec")
  (aluminium2  "#d3d7cf")
  (aluminium3  "#babdb6")
  (aluminium4  "#888a85")
  (aluminium5  "#555753")
  (aluminium6  "#2e3436"))

(custom-theme-set-faces
  'oblivion

  ;;; define some reusable faces that we can inherit from afterwards
  `(strong-face ((,class (:weight bold))))
  `(warning-face ((,class (:foreground ,orange3 :weight bold :underline t))))
  `(error-face ((,class (:foreground ,scarletred3 :weight bold :underline t))))

  ;;; basic coloring
  `(default ((,class (:foreground ,aluminium2, :background ,aluminium6))))
  `(cursor ((,class (:background ,aluminium2))))
  `(escape-glyph-face ((,class (:foreground ,butter2))))
  `(fringe ((,class (:foreground ,aluminium2 :background ,aluminium6))))
  `(header-line ((,class (:foreground ,aluminium2 :background ,aluminium5))))
  `(highlight ((,class (:background ,aluminium5))))

  ;; faces used by isearch
  `(isearch ((,class (:foreground ,aluminium6 :background ,aluminium1))))
  `(isearch-fail ((,class (:foreground ,aluminium2 :background ,scarletred3))))
  `(lazy-highlight ((,class (:foreground ,aluminium1 :background ,chameleon3))))

  `(menu ((,class (:foreground ,aluminium2 :background ,aluminium6))))
  `(minibuffer-prompt ((,class (:foreground ,skyblue1))))
  `(mode-line((,class (:foreground ,aluminium6 :background ,aluminium1))))
  `(mode-line-buffer-id ((,class (:inherit strong-face))))
  `(mode-line-inactive ((,class (:foreground ,aluminium2 :background ,aluminium5))))
  `(region ((,class (:foreground ,aluminium1 :background ,aluminium4))))
  `(secondary-selection ((,class (:foreground ,aluminium1 :background ,skyblue1))))
  `(trailing-whitespace ((,class (:background ,butter2))))
  `(vertical-border ((,class (:foreground ,aluminium2))))

  ;;; font lock
  `(font-lock-builtin-face ((,class (:foreground ,skyblue1))))
  `(font-lock-comment-face ((,class (:foreground ,aluminium4 :slant italic))))
  `(font-lock-comment-delimiter-face ((,class (:foreground ,aluminium4))))
  `(font-lock-constant-face ((,class (:foreground ,orange2))))
  `(font-lock-doc-face ((,class (:foreground ,butter2 :slant italic))))
  `(font-lock-function-name-face ((,class (:foreground ,aluminium2))))
  `(font-lock-keyword-face ((,class (:foreground ,aluminium1 :weight bold))))
  `(font-lock-negation-char-face ((,class (:foreground ,aluminium2))))
  `(font-lock-preprocessor-face ((,class (:foreground ,plum1))))
  `(font-lock-string-face ((,class (:foreground ,butter2))))
  `(font-lock-type-face ((,class (:foreground ,chameleon1 :weight bold))))
  `(font-lock-variable-name-face ((,class (:foreground ,aluminium2))))
  `(font-lock-warning-face ((,class (:inherit warning-face))))

  `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

  ;; auto-complete
  `(ac-candidate-face ((,class (:background ,aluminium1 :foreground "black"))))
  `(ac-selection-face ((,class (:background ,skyblue2 :foreground ,aluminium1))))
  `(popup-tip-face ((,class (:background ,butter1 :foreground "black"))))
  `(popup-scroll-bar-foreground-face ((,class (:background ,skyblue1))))
  `(popup-scroll-bar-background-face ((,class (:background ,aluminium5))))
  `(popup-isearch-match ((,class (:background ,aluminium6 :foreground ,aluminium2))))

  ;; diff
  `(diff-added ((,class (:foreground ,chameleon1))))
  `(diff-changed ((,class (:foreground ,butter1))))
  `(diff-removed ((,class (:foreground ,scarletred1))))
  `(diff-header ((,class (:background ,aluminium5))))
  `(diff-file-header ((,class (:background ,skyblue3 :foreground ,aluminium1 :bold t))))

  ;; eshell
  `(eshell-prompt ((,class (:inherit strong-face))))
  `(eshell-ls-archive ((,class (:foreground ,scarletred1 :weight bold))))
  `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
  `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
  `(eshell-ls-directory ((,class (:foreground ,skyblue1 :weight bold))))
  `(eshell-ls-executable ((,class (:foreground ,chameleon2 :weight bold))))
  `(eshell-ls-unreadable ((,class (:foreground ,aluminium2))))
  `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
  `(eshell-ls-product ((,class (:inherit font-lock-doc))))
  `(eshell-ls-special ((,class (:inherit strong-face))))
  `(eshell-ls-symlink ((,class (:foreground ,plum1 :weight bold))))

  ;; flymake
  `(flymake-errline ((,class (:inherit error-face))))
  `(flymake-warnline ((,class (:inherit warning-face))))

  ;; flyspell
  `(flyspell-duplicate ((,class (:inherit warning-face))))
  `(flyspell-incorrect ((,class (:inherit error-face))))

  ;; hl-line-mode
  `(hl-line-face ((,class (:background ,aluminium5))))

  ;; ido-mode
  `(ido-first-match ((,class (:inherit strong-face))))
  `(ido-only-match ((,class (:inherit strong-face))))
  `(ido-subdir ((,class (:foreground ,aluminium3))))

  ;; js2-mode
  `(js2-warning-face ((,class (:underline ,orange1))))
  `(js2-error-face ((,class (:inherit error-face))))
  `(js2-jsdoc-tag-face ((,class (:foreground ,chameleon1))))
  `(js2-jsdoc-type-face ((,class (:foreground ,orange2))))
  `(js2-jsdoc-value-face ((,class (:foreground ,aluminium1 :weight bold))))
  `(js2-function-param-face ((,class (:foreground ,orange1 :slant italic))))
  `(js2-jsdoc-html-tag-name-face ((,class (:foreground ,skyblue1))))
  `(js2-jsdoc-html-tag-delimiter-face ((,class (:foreground ,skyblue1))))
  `(js2-external-variable-face ((,class (:foreground ,orange2))))

  ;; linum-mode
  `(line-number ((,class (:foreground ,aluminium5))))
  `(linum ((,class (:foreground ,aluminium5))))

  ;; magit
  `(magit-section-title ((,class (:inherit strong-face))))
  `(magit-branch ((,class (:inherit strong-face))))

  ;; nxhtml
  `(nxml-tag-delimiter ((,class (:foreground ,skyblue1))))
  `(nxml-tag-delimiter-face ((,class (:foreground ,skyblue1))))

  ;; css-mode
  `(css-property ((,class (:inherit bold :foreground "#ffffff"))))
  `(css-selector ((,class (:foreground "#d3d7cf"))))
  
  ;; mumamo
  `(mumamo-background-chunk-major ((,class (:background ,aluminium6))))
  `(mumamo-background-chunk-submode1 ((,class (:background ,aluminium6))))
  `(mumamo-background-chunk-submode2 ((,class (:background ,aluminium6))))
  `(mumamo-background-chunk-submode3 ((,class (:background ,aluminium6))))
  `(mumamo-background-chunk-submode4 ((,class (:background ,aluminium6))))
  `(mumamo-background-chunk-submode5 ((,class (:background ,aluminium6))))

  ;; outline
  `(outline-8 ((,class (:inherit default))))
  `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
  `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
  `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
  `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
  `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
  `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
  `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

  ;; show-paren
  `(show-paren-mismatch ((,class (:foreground ,aluminium1 :weight bold :background ,aluminium3))))
  `(show-paren-match ((,class (:foreground ,aluminium1 :weight bold :background ,aluminium3)))))

  (custom-theme-set-variables
   'oblivion
   `(ansi-color-names-vector ["black" "red1" "green3" "yellow2" "skyblue2" "magenta3" "cyan4" "aluminium1"])))

(provide-theme 'oblivion)
