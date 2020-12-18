;;; pink-cat-boo-buffy.el --- Pink Cat Boo theme, Buffified 

;; Copyright 2015-present, All rights reserved
;;
;; Code licensed under the MIT license

;;; Code:
(require 'cl-lib)
(deftheme pink-cat-boo-buffy)


;;;; Configuration options:

(defgroup pink-cat-boo-buffy nil
  "Pink Cat Boo theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom pink-cat-boo-buffy-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'pink-cat-boo-buffy)

(defcustom pink-cat-boo-buffy-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'pink-cat-boo-buffy)

(defcustom pink-cat-boo-buffy-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'pink-cat-boo-buffy)

(defcustom pink-cat-boo-buffy-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'pink-cat-boo-buffy)

(defcustom pink-cat-boo-buffy-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'pink-cat-boo-buffy)

(defcustom pink-cat-boo-buffy-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boolean
  :group 'pink-cat-boo-buffy)

(defvar pink-cat-boo-buffy-use-24-bit-colors-on-256-colors-terms nil
  "Use true colors even on terminals announcing less capabilities.

Beware the use of this variable.  Using it may lead to unwanted
behavior, the most common one being an ugly blue background on
terminals, which don't understand 24 bit colors.  To avoid this
blue background, when using this variable, one can try to add the
following lines in their config file after having load the
Pink-Cat-Boo-Buffy theme:

    (unless (display-graphic-p)
      (set-face-background 'default \"black\" nil))

There is a lot of discussion behind the 256 colors theme (see URL
`https://github.com/pink-cat-boo-buffy/emacs/pull/57').  Please take time to
read it before opening a new issue about your will.")


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (pink-cat-boo-buffy-bg      "#1F1D23" "unspecified-bg" "unspecified-bg") ; official background
                ;; (pink-cat-boo-buffy-bg      "#202330" "unspecified-bg" "unspecified-bg") ; official background
					 (pink-cat-boo-buffy-dark    "#1e1e1e" "unspecified-bg" "unspecified-bg")
                ;; (pink-cat-boo-buffy-fg      "#FFF0F5" "#ffffff" "brightwhite") ; official foreground
                (pink-cat-boo-buffy-fg      "#EBA4AC" "#ffffff" "brightwhite") ; official foreground
                (pink-cat-boo-buffy-current "#472541" "#303030" "brightblack") ; official current-line/selection
                ;; (pink-cat-boo-buffy-current "#44475a" "#303030" "brightblack") ; official current-line/selection
                (pink-cat-boo-buffy-comment "#A16193" "#5f5faf" "blue")	; official comment
                (pink-cat-boo-buffy-cyan    "#A2C2EB" "#87d7ff" "brightcyan")	; official cyan
                (pink-cat-boo-buffy-green   "#58B896" "#5fff87" "green") ; official green
                (pink-cat-boo-buffy-orange  "ffc85b" "#ffaf5f" "brightred") ; official orange
                ;; (pink-cat-boo-buffy-pink    "#E33878" "#ff87d7" "magenta")     ; official pink
                (pink-cat-boo-buffy-pink    "#EA4183" "#ff87d7" "magenta")	; official pink
                ;; (pink-cat-boo-buffy-purple  "#C5C8C6" "#af87ff" "brightmagenta") ; official purple
                (pink-cat-boo-buffy-purple  "#DCBFF2" "#af87ff" "brightmagenta") ; official purple
                (pink-cat-boo-buffy-red     "#ff62a5" "#ff8787" "red") ; official red
                (pink-cat-boo-buffy-yellow  "#FAE8B6" "#ffff87" "yellow") ; official yellow
                (pink-cat-boo-buffy-highlight  "#d0963a" "#ffff87" "yellow") ; official yellow
                ;; Other colors
                (bg2             "#2D2F42" "#121212" "brightblack")
                (bg3             "#3A3D54" "#262626" "brightblack")
                (bg4             "#1E1320" "#444444" "brightblack")
                (fg2             "#FFF0F5" "#e4e4e4" "brightwhite")
                (fg3             "#4747a1" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#A2C2EB" "#0087ff" "brightblue")
					 (other-green     "9CD162" "#0087ff" "brightblue")))
      (faces '(;; default
               (cursor :background ,fg4)
               (completions-first-difference :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (default :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-fg)
               (default-italic :slant italic)
               (ffap :foreground ,fg4)
               (fringe :background ,pink-cat-boo-buffy-bg :foreground ,fg4)
               (highlight :background ,pink-cat-boo-buffy-dark)
               (hl-line :background ,pink-cat-boo-buffy-current :extend t)
               (region :background "#31243B")
               (selectrum-current-candidate :background ,pink-cat-boo-buffy-current)
					(grugru-highlight-face :box (:color ,pink-cat-boo-buffy-pink :line-width -1 :style nil) :background ,pink-cat-boo-buffy-dark :foreground ,pink-cat-boo-buffy-highlight)
					;; (marginalia-documentation :background ,pink-cat-boo-buffy-bg :foregound ,pink-cat-boo-buffy-comment)
               ;; (marginalia-lighter :background ,pink-cat-boo-buffy-bg :foregound ,pink-cat-boo-buffy-comment)
               ;; (marginalia-annotate-face :foregound ,pink-cat-boo-buffy-comment :background ,pink-cat-boo-buffy-bg)
               (marginalia-documentation :foregound ,pink-cat-boo-buffy-comment :background ,pink-cat-boo-buffy-bg :slant italic)
               (info-quoted-name :foreground ,pink-cat-boo-buffy-orange)
               (info-string :foreground ,pink-cat-boo-buffy-yellow)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,pink-cat-boo-buffy-cyan :underline t)
               (linum :slant italic :foreground ,bg4 :background ,pink-cat-boo-buffy-bg)
               (line-number :slant italic :foreground ,pink-cat-boo-buffy-comment :background ,pink-cat-boo-buffy-bg)
               (match :background ,pink-cat-boo-buffy-yellow :foreground ,pink-cat-boo-buffy-bg)
               (minibuffermpt
                ,@(if pink-cat-boo-buffy-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground pink-cat-boo-buffy-fg)
                    (list :weight 'bold :foreground pink-cat-boo-buffy-pink)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (trailing-whitespace :foreground "unspecified-fg" :background ,pink-cat-boo-buffy-orange)
               (vertical-border :foreground ,bg2)
               (success :foreground ,pink-cat-boo-buffy-green)
               (warning :foreground ,pink-cat-boo-buffy-orange)
               (error :foreground ,pink-cat-boo-buffy-red)
               (header-line :background ,pink-cat-boo-buffy-bg)
               (tab-line :background ,bg4)
               ;; syntax
               (font-lock-builtin-face :foreground ,pink-cat-boo-buffy-orange)
               (font-lock-comment-face :foreground ,pink-cat-boo-buffy-comment)
               (font-lock-comment-delimiter-face :foreground ,pink-cat-boo-buffy-comment)
               (font-lock-constant-face :foreground ,pink-cat-boo-buffy-cyan)
               (font-lock-doc-face :foreground ,pink-cat-boo-buffy-comment)
               (font-lock-function-name-face :foreground ,pink-cat-boo-buffy-green :weight bold)
               (font-lock-keyword-face :weight bold :foreground ,pink-cat-boo-buffy-pink)
               (font-lock-negation-char-face :foreground ,pink-cat-boo-buffy-cyan)
               (font-lock-preprocessor-face :foreground ,pink-cat-boo-buffy-orange)
               (font-lock-reference-face :foreground ,pink-cat-boo-buffy-cyan)
               (font-lock-regexp-grouping-backslash :foreground ,pink-cat-boo-buffy-cyan)
               (font-lock-regexp-grouping-construct :foreground ,pink-cat-boo-buffy-purple)
               (font-lock-string-face :foreground ,pink-cat-boo-buffy-yellow)
               (font-lock-type-face :foreground ,pink-cat-boo-buffy-purple)
               (font-lock-variable-name-face :foreground ,pink-cat-boo-buffy-fg
                                             :weight bold)
               (font-lock-warning-face :foreground ,pink-cat-boo-buffy-orange :background ,bg2)
					;; company
               (company-echo-common :foreground ,pink-cat-boo-buffy-bg :background ,pink-cat-boo-buffy-fg)
               (company-preview :background ,pink-cat-boo-buffy-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,pink-cat-boo-buffy-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,pink-cat-boo-buffy-green)
               (company-scrollbar-bg :background ,pink-cat-boo-buffy-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,pink-cat-boo-buffy-fg :background ,pink-cat-boo-buffy-dark)
               (company-tooltip-search :foreground ,pink-cat-boo-buffy-green
                                       :underline t)
               (company-tooltip-search-selection :background ,pink-cat-boo-buffy-green
                                                 :foreground ,pink-cat-boo-buffy-bg)
               (company-tooltip-selection :inherit match :background ,pink-cat-boo-buffy-current :foreground ,pink-cat-boo-buffy-fg)
               ;; (company-tooltip-mouse :background ,pink-cat-boo-buffy-bg)
               (company-tooltip-common :foreground ,pink-cat-boo-buffy-pink :weight bold)
               ;;(company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-annotation :foreground ,pink-cat-boo-buffy-cyan)
               ;;(company-tooltip-annotation-selection :inherit company-tooltip-annotation)
					(company-box-background :background ,pink-cat-boo-buffy-dark)
					(company-box-tooltip :background ,pink-cat-boo-buffy-dark)
					(company-box-numbers :foreground ,pink-cat-boo-buffy-comment :slant italic)
					(company-box-selection :background ,pink-cat-boo-buffy-current :foreground ,pink-cat-boo-buffy-current)
					(company-box-scrollbar :background ,pink-cat-boo-buffy-comment :foreground ,pink-cat-boo-buffy-comment)
					(company-box-backends-colors '(
															 (company-tabnine . (:candidate ,pink-cat-boo-buffy-orange))))
               ;; diff-hl
               (diff-hl-change :foreground ,pink-cat-boo-buffy-orange :background ,pink-cat-boo-buffy-orange)
               (diff-hl-delete :foreground ,pink-cat-boo-buffy-red :background ,pink-cat-boo-buffy-red)
               (diff-hl-insert :foreground ,pink-cat-boo-buffy-green :background ,pink-cat-boo-buffy-green)
               ;; dired
               (dired-directory :foreground ,pink-cat-boo-buffy-green :weight normal)
               (dired-flagged :foreground ,pink-cat-boo-buffy-pink)
               (dired-header :foreground ,fg3 :background ,pink-cat-boo-buffy-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,pink-cat-boo-buffy-fg :weight bold)
               (dired-marked :foreground ,pink-cat-boo-buffy-orange :weight bold)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,pink-cat-boo-buffy-yellow :weight normal :slant italic)
               (dired-warning :foreground ,pink-cat-boo-buffy-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,pink-cat-boo-buffy-fg)
               (diredp-deletion-file-name :foreground ,pink-cat-boo-buffy-pink :background ,pink-cat-boo-buffy-current)
               (diredp-deletion :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,pink-cat-boo-buffy-orange)
               (diredp-file-name :foreground ,pink-cat-boo-buffy-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,pink-cat-boo-buffy-current)
               (diredp-flag-mark :foreground ,fg2 :weight bold :background ,pink-cat-boo-buffy-current)
               (diredp-ignored-file-name :foreground ,pink-cat-boo-buffy-fg)
               (diredp-mode-line-flagged :foreground ,pink-cat-boo-buffy-orange)
               (diredp-mode-line-marked :foreground ,pink-cat-boo-buffy-orange)
               (diredp-no-priv :foreground ,pink-cat-boo-buffy-fg)
               (diredp-number :foreground ,pink-cat-boo-buffy-cyan)
               (diredp-other-priv :foreground ,pink-cat-boo-buffy-orange)
               (diredp-rare-priv :foreground ,pink-cat-boo-buffy-orange)
               (diredp-read-priv :foreground ,pink-cat-boo-buffy-purple)
               (diredp-write-priv :foreground ,pink-cat-boo-buffy-pink)
               (diredp-exec-priv :foreground ,pink-cat-boo-buffy-yellow)
               (diredp-symlink :foreground ,pink-cat-boo-buffy-orange)
               (diredp-link-priv :foreground ,pink-cat-boo-buffy-orange)
               (diredp-autofile-name :foreground ,pink-cat-boo-buffy-yellow)
               (diredp-tagged-autofile-name :foreground ,pink-cat-boo-buffy-yellow)
               ;; elfeed
               (elfeed-search-date-face :foreground ,pink-cat-boo-buffy-comment)
               (elfeed-search-title-face :foreground ,pink-cat-boo-buffy-fg)
               (elfeed-search-unread-title-face :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (elfeed-search-feed-face :foreground ,pink-cat-boo-buffy-fg :weight bold)
               (elfeed-search-tag-face :foreground ,pink-cat-boo-buffy-green)
               (elfeed-search-last-update-face :weight bold)
               (elfeed-search-unread-count-face :foreground ,pink-cat-boo-buffy-pink)
               (elfeed-search-filter-face :foreground ,pink-cat-boo-buffy-green :weight bold)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,pink-cat-boo-buffy-red)
               (elfeed-log-warn-level-face :foreground ,pink-cat-boo-buffy-orange)
               (elfeed-log-info-level-face :foreground ,pink-cat-boo-buffy-cyan)
               (elfeed-log-debug-level-face :foreground ,pink-cat-boo-buffy-comment)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,pink-cat-boo-buffy-yellow)
               (enh-ruby-op-face :foreground ,pink-cat-boo-buffy-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,pink-cat-boo-buffy-yellow)
               (enh-ruby-string-delimiter-face :foreground ,pink-cat-boo-buffy-yellow)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,pink-cat-boo-buffy-orange))
               (flyspell-incorrect :underline (:style wave :color ,pink-cat-boo-buffy-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,pink-cat-boo-buffy-purple)
               (font-latex-italic-face :foreground ,pink-cat-boo-buffy-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,pink-cat-boo-buffy-cyan)
               (font-latex-match-variable-keywords :foreground ,pink-cat-boo-buffy-fg)
               (font-latex-string-face :foreground ,pink-cat-boo-buffy-yellow)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,pink-cat-boo-buffy-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,pink-cat-boo-buffy-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,pink-cat-boo-buffy-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,pink-cat-boo-buffy-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,pink-cat-boo-buffy-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,pink-cat-boo-buffy-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,pink-cat-boo-buffy-pink)
               (gnus-header-from :foreground ,pink-cat-boo-buffy-fg)
               (gnus-header-name :foreground ,pink-cat-boo-buffy-purple)
               (gnus-header-subject :foreground ,pink-cat-boo-buffy-green :weight bold)
               (gnus-summary-markup-face :foreground ,pink-cat-boo-buffy-cyan)
               (gnus-summary-high-unread :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,pink-cat-boo-buffy-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (gnus-summary-low-unread :foreground ,pink-cat-boo-buffy-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,pink-cat-boo-buffy-pink)
               (haskell-constructor-face :foreground ,pink-cat-boo-buffy-purple)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; lsp
               (lsp-ui-peek-peek :background ,pink-cat-boo-buffy-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,pink-cat-boo-buffy-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,pink-cat-boo-buffy-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-footer :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,pink-cat-boo-buffy-fg :weight ultra-bold
                                               :box (:line-width -1 :color pink-cat-boo-buffy-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,pink-cat-boo-buffy-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,pink-cat-boo-buffy-bg)
               (lsp-ui-doc-header :foreground ,pink-cat-boo-buffy-bg :background ,pink-cat-boo-buffy-cyan)
               ;; magit
               (magit-branch-local :foreground ,pink-cat-boo-buffy-cyan)
               (magit-branch-remote :foreground ,pink-cat-boo-buffy-green)
               (magit-tag :foreground ,pink-cat-boo-buffy-orange)
               (magit-section-heading :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,pink-cat-boo-buffy-orange
                                            :background ,pink-cat-boo-buffy-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,pink-cat-boo-buffy-orange
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,pink-cat-boo-buffy-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,pink-cat-boo-buffy-green)
               (magit-diffstat-removed :foreground ,pink-cat-boo-buffy-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,pink-cat-boo-buffy-orange :weight bold)
               (magit-process-ok :foreground ,pink-cat-boo-buffy-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,pink-cat-boo-buffy-orange)
               (markdown-code-face :foreground ,pink-cat-boo-buffy-orange)
               (markdown-footnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,pink-cat-boo-buffy-pink
                ,@(when pink-cat-boo-buffy-enlarge-headings
                    (list :height pink-cat-boo-buffy-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,pink-cat-boo-buffy-purple
                ,@(when pink-cat-boo-buffy-enlarge-headings
                    (list :height pink-cat-boo-buffy-height-title-2)))
               (markdown-header-face-3
                :foreground ,pink-cat-boo-buffy-green
                ,@(when pink-cat-boo-buffy-enlarge-headings
                    (list :height pink-cat-boo-buffy-height-title-3)))
               (markdown-header-face-4 :foreground ,pink-cat-boo-buffy-yellow)
               (markdown-header-face-5 :foreground ,pink-cat-boo-buffy-cyan)
               (markdown-header-face-6 :foreground ,pink-cat-boo-buffy-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,pink-cat-boo-buffy-fg)
               (markdown-inline-code-face :foreground ,pink-cat-boo-buffy-yellow)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,pink-cat-boo-buffy-orange)
               (markdown-table-face :foreground ,pink-cat-boo-buffy-purple)
               ;; message
               (message-header-to :foreground ,pink-cat-boo-buffy-fg :weight bold)
               (message-header-cc :foreground ,pink-cat-boo-buffy-fg :bold bold)
               (message-header-subject :foreground ,pink-cat-boo-buffy-orange)
               (message-header-newsgroups :foreground ,pink-cat-boo-buffy-purple)
               (message-header-other :foreground ,pink-cat-boo-buffy-purple)
               (message-header-name :foreground ,pink-cat-boo-buffy-green)
               (message-header-xheader :foreground ,pink-cat-boo-buffy-cyan)
               (message-separator :foreground ,pink-cat-boo-buffy-cyan :slant italic)
               (message-cited-text :foreground ,pink-cat-boo-buffy-purple)
               (message-cited-text-1 :foreground ,pink-cat-boo-buffy-purple)
               (message-cited-text-2 :foreground ,pink-cat-boo-buffy-orange)
               (message-cited-text-3 :foreground ,pink-cat-boo-buffy-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,pink-cat-boo-buffy-green :weight normal)
               ;; mode-line
               (mode-line :background ,pink-cat-boo-buffy-current
                          :box ,pink-cat-boo-buffy-current :inverse-video nil
                          ,@(if pink-cat-boo-buffy-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground "unspecified-fg")))
               (mode-line-inactive
                :inverse-video nil
                ,@(if pink-cat-boo-buffy-alternate-mode-line-and-minibuffer
                      (list :foreground pink-cat-boo-buffy-comment :background pink-cat-boo-buffy-bg
                            :box pink-cat-boo-buffy-bg)
                    (list :foreground pink-cat-boo-buffy-fg :background bg2 :box bg2)))
               ;; mu4e
               (mu4e-unread-face :foreground ,pink-cat-boo-buffy-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,pink-cat-boo-buffy-purple)
               (mu4e-highlight-face :background ,pink-cat-boo-buffy-bg
                                    :foreground ,pink-cat-boo-buffy-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,pink-cat-boo-buffy-current
                                           :foreground ,pink-cat-boo-buffy-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,pink-cat-boo-buffy-purple)
               (mu4e-cited-1-face :foreground ,pink-cat-boo-buffy-purple)
               (mu4e-cited-2-face :foreground ,pink-cat-boo-buffy-orange)
               (mu4e-cited-3-face :foreground ,pink-cat-boo-buffy-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; org
               (org-agenda-date :foreground ,pink-cat-boo-buffy-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,pink-cat-boo-buffy-comment)
               (org-agenda-done :foreground ,pink-cat-boo-buffy-green)
               (org-agenda-structure :foreground ,pink-cat-boo-buffy-purple)
               ;; (org-block :foreground ,pink-cat-boo-buffy-orange)
               ;; (org-code :foreground ,pink-cat-boo-buffy-yellow)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,pink-cat-boo-buffy-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,pink-cat-boo-buffy-comment)
               (org-document-title :weight bold :foreground ,pink-cat-boo-buffy-orange
                                   ,@(when pink-cat-boo-buffy-enlarge-headings
                                       (list :height pink-cat-boo-buffy-height-doc-title)))
               (org-done :box (:color ,fg4 :line-width -1 :style nil) :foreground ,pink-cat-boo-buffy-green :background ,pink-cat-boo-buffy-dark)
               (org-todo :box (:color ,fg4 :line-width -1 :style nil) :foreground ,pink-cat-boo-buffy-orange :background ,pink-cat-boo-buffy-dark)
               ;; (org-todo :foreground ,pink-cat-boo-buffy-orange :weight bold)
               (org-ellipsis :foreground ,pink-cat-boo-buffy-comment)
               (org-footnote :foreground ,other-blue)
               (org-formula :foreground ,pink-cat-boo-buffy-pink)
               (org-headline-done :foreground ,pink-cat-boo-buffy-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,pink-cat-boo-buffy-bg :background ,pink-cat-boo-buffy-bg)
               (org-level-1 :inherit bold :foreground ,pink-cat-boo-buffy-pink
                            ,@(when pink-cat-boo-buffy-enlarge-headings
                                (list :height pink-cat-boo-buffy-height-title-1)))
               (org-level-2 :inherit bold :foreground ,pink-cat-boo-buffy-purple
                            ,@(when pink-cat-boo-buffy-enlarge-headings
                                (list :height pink-cat-boo-buffy-height-title-2)))
               (org-level-3 :weight normal :foreground ,pink-cat-boo-buffy-green
                            ,@(when pink-cat-boo-buffy-enlarge-headings
                                (list :height pink-cat-boo-buffy-height-title-3)))
               (org-level-4 :weight normal :foreground ,pink-cat-boo-buffy-yellow)
               (org-level-5 :weight normal :foreground ,pink-cat-boo-buffy-cyan)
               (org-level-6 :weight normal :foreground ,pink-cat-boo-buffy-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,pink-cat-boo-buffy-fg)
               (org-link :foreground ,pink-cat-boo-buffy-cyan :underline t)
               (org-priority :foreground ,pink-cat-boo-buffy-cyan)
               (org-scheduled :foreground ,pink-cat-boo-buffy-green)
               (org-scheduled-previously :foreground ,pink-cat-boo-buffy-yellow)
               (org-scheduled-today :foreground ,pink-cat-boo-buffy-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,pink-cat-boo-buffy-yellow)
               (org-table :foreground ,pink-cat-boo-buffy-purple)
               (org-tag :foreground ,pink-cat-boo-buffy-pink :weight bold :background ,bg2)
                (org-upcoming-deadline :foreground ,pink-cat-boo-buffy-yellow)
               (org-warning :weight bold :foreground ,pink-cat-boo-buffy-pink)
               ;; outline
               (outline-1 :foreground ,pink-cat-boo-buffy-pink)
               (outline-2 :foreground ,pink-cat-boo-buffy-purple)
               (outline-3 :foreground ,pink-cat-boo-buffy-green)
               (outline-4 :foreground ,pink-cat-boo-buffy-yellow)
               (outline-5 :foreground ,pink-cat-boo-buffy-cyan)
               (outline-6 :foreground ,pink-cat-boo-buffy-orange)
               ;; powerline
               (powerline-active1 :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-pink)
               (powerline-active2 :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,pink-cat-boo-buffy-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,pink-cat-boo-buffy-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,pink-cat-boo-buffy-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,pink-cat-boo-buffy-comment)
               (rainbow-delimiters-depth-2-face :foreground ,pink-cat-boo-buffy-cyan)
               ;; (rainbow-delimiters-depth-3-face :foreground ,pink-cat-boo-buffy-purple)
               (rainbow-delimiters-depth-3-face :foreground ,pink-cat-boo-buffy-pink)
               ;; (rainbow-delimiters-depth-4-face :foreground ,pink-cat-boo-buffy-orange)
               ;; (rainbow-delimiters-depth-6-face :foreground ,pink-cat-boo-buffy-green)
               (rainbow-delimiters-depth-4-face :foreground ,pink-cat-boo-buffy-orange)
               (rainbow-delimiters-depth-5-face :foreground ,other-blue)
               ;; (rainbow-delimiters-unmatched-face :foreground ,pink-cat-boo-buffy-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,pink-cat-boo-buffy-green)
               (rpm-spec-doc-face :foreground ,pink-cat-boo-buffy-pink)
               (rpm-spec-ghost-face :foreground ,pink-cat-boo-buffy-purple)
               (rpm-spec-macro-face :foreground ,pink-cat-boo-buffy-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,pink-cat-boo-buffy-purple)
               (rpm-spec-section-face :foreground ,pink-cat-boo-buffy-yellow)
               (rpm-spec-tag-face :foreground ,pink-cat-boo-buffy-cyan)
               (rpm-spec-var-face :foreground ,pink-cat-boo-buffy-orange)
               ;; selectrum-mode
               (selectrum-current-candidate :foreground ,pink-cat-boo-buffy-pink)
               (selectrum-primary-highlight :foreground ,pink-cat-boo-buffy-orange)
               (selectrum-secondary-highlight :foreground ,pink-cat-boo-buffy-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,pink-cat-boo-buffy-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,pink-cat-boo-buffy-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,pink-cat-boo-buffy-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,pink-cat-boo-buffy-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,pink-cat-boo-buffy-green)
               (speedbar-file-face :foreground ,pink-cat-boo-buffy-cyan)
               (speedbar-directory-face :foreground ,pink-cat-boo-buffy-purple)
               (speedbar-tag-face :foreground ,pink-cat-boo-buffy-yellow)
               (speedbar-selected-face :foreground ,pink-cat-boo-buffy-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,pink-cat-boo-buffy-bg
                                        :foreground ,pink-cat-boo-buffy-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,pink-cat-boo-buffy-purple :background ,pink-cat-boo-buffy-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,pink-cat-boo-buffy-pink :background ,pink-cat-boo-buffy-bg
                            :box (:line-width 2 :color ,pink-cat-boo-buffy-bg :style nil))
               (tab-bar-tab-inactive :foreground ,pink-cat-boo-buffy-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,pink-cat-boo-buffy-purple :background ,pink-cat-boo-buffy-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,pink-cat-boo-buffy-pink :background ,pink-cat-boo-buffy-bg
                             :box (:line-width 2 :color ,pink-cat-boo-buffy-bg :style nil))
               (tab-line-tab-inactive :foreground ,pink-cat-boo-buffy-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,pink-cat-boo-buffy-red)
               ;; term
               (term :foreground ,pink-cat-boo-buffy-fg :background ,pink-cat-boo-buffy-bg)
               (term-color-black :foreground ,pink-cat-boo-buffy-bg :background ,pink-cat-boo-buffy-bg)
               (term-color-blue :foreground ,pink-cat-boo-buffy-purple :background ,pink-cat-boo-buffy-purple)
               (term-color-cyan :foreground ,pink-cat-boo-buffy-cyan :background ,pink-cat-boo-buffy-cyan)
               (term-color-green :foreground ,pink-cat-boo-buffy-green :background ,pink-cat-boo-buffy-green)
               (term-color-magenta :foreground ,pink-cat-boo-buffy-pink :background ,pink-cat-boo-buffy-pink)
               (term-color-red :foreground ,pink-cat-boo-buffy-red :background ,pink-cat-boo-buffy-red)
               (term-color-white :foreground ,pink-cat-boo-buffy-fg :background ,pink-cat-boo-buffy-fg)
               (term-color-yellow :foreground ,pink-cat-boo-buffy-yellow :background ,pink-cat-boo-buffy-yellow)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,pink-cat-boo-buffy-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,pink-cat-boo-buffy-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,pink-cat-boo-buffy-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit ,font-lock-builtin-face)
               (web-mode-comment-face :inherit ,font-lock-comment-face)
               (web-mode-constant-face :inherit ,font-lock-constant-face)
               (web-mode-doctype-face :inherit ,font-lock-comment-face)
               (web-mode-function-name-face :inherit ,font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,pink-cat-boo-buffy-purple)
               (web-mode-html-attr-value-face :foreground ,pink-cat-boo-buffy-green)
               (web-mode-html-tag-face :foreground ,pink-cat-boo-buffy-pink :weight bold)
               (web-mode-keyword-face :foreground ,pink-cat-boo-buffy-pink)
               (web-mode-string-face :foreground ,pink-cat-boo-buffy-yellow)
               (web-mode-type-face :inherit ,font-lock-type-face)
               (web-mode-warning-face :inherit ,font-lock-warning-face)
               ;; which-func
               (which-func :inherit ,font-lock-function-name-face)
               ;; whitespace
               (whitespace-big-indent :background ,pink-cat-boo-buffy-red :foreground ,pink-cat-boo-buffy-red)
               (whitespace-empty :background ,pink-cat-boo-buffy-orange :foreground ,pink-cat-boo-buffy-red)
               (whitespace-hspace :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-comment)
               (whitespace-indentation :background ,pink-cat-boo-buffy-orange :foreground ,pink-cat-boo-buffy-red)
               (whitespace-line :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-pink)
               ;; (whitespace-newline :foreground ,pink-cat-boo-buffy-comment)
               ;; (whitespace-space :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-comment)
               (whitespace-space-after-tab :background ,pink-cat-boo-buffy-orange :foreground ,pink-cat-boo-buffy-red)
               (whitespace-space-before-tab :background ,pink-cat-boo-buffy-orange :foreground ,pink-cat-boo-buffy-red)
               ;; (whitespace-tab :background ,bg2 :foreground ,pink-cat-boo-buffy-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit ,font-lock-builtin-face)
               (yard-directive-face :inherit ,font-lock-builtin-face)
               ;; mark / region
               (region :background ,pink-cat-boo-buffy-pink :foreground ,pink-cat-boo-buffy-pink)
               ;; linum
               (linum-relative-current-face :foreground ,fg4)
               (linum-number-current-line :background ,pink-cat-boo-buffy-pink :foreground ,fg4)
					;; Whitespace custom
					(font-lock-comment-face :slant italic)
					(font-lock-keyword-face :slant italic)
					(whitespace-tab :background ,pink-cat-boo-buffy-bg :foreground ,pink-cat-boo-buffy-bg)	; Do not highlight indentation.
					(whitespace-space :foreground ,pink-cat-boo-buffy-bg)
					(whitespace-newline :foreground ,pink-cat-boo-buffy-bg :height 80)
					;; Centaur Tabs
					(centaur-tabs-selected :background ,pink-cat-boo-buffy-bg :foreground ,fg4 :height 80)
					(centaur-tabs-selected-modified :background ,pink-cat-boo-buffy-bg :foreground ,fg2 :height 80)
					(centaur-tabs-unselected :background ,pink-cat-boo-buffy-dark :foreground ,fg4 :height 80)
					(centaur-tabs-unselected-modified :background ,pink-cat-boo-buffy-dark :foreground ,fg2 :height 80)
					(centaur-tabs-default :background ,pink-cat-boo-buffy-dark)
					(centaur-tabs-active-bar-face :background ,pink-cat-boo-buffy-dark)
					;; TODO: Fix
					(highlight-indent-guides-character-face :foreground ,pink-cat-boo-buffy-comment)
					(highlight-indent-guides-top-character-face :foreground ,pink-cat-boo-buffy-orange)
					;;
					;; (highlight-thing :background ,bg3 :foreground ,fg4)
					;; Tree Sitter
					(tree-sitter-hl-face:function :foreground ,pink-cat-boo-buffy-cyan :weight Book :slant italic :family "Operator Mono Lig")
					(tree-sitter-hl-face:function.call :foreground ,pink-cat-boo-buffy-orange :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:function.builtin :foreground ,pink-cat-boo-buffy-cyan :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:function.macro :foreground ,pink-cat-boo-buffy-green :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					
					;; (tree-sitter-hl-face:variable :foreground ,fg2)
					(tree-sitter-hl-face:variable :foreground ,pink-cat-boo-buffy-fg)
					(tree-sitter-hl-face:variable.builtin :foreground ,pink-cat-boo-buffy-green)
					(tree-sitter-hl-face:variable.parameter :foreground ,fg4)
					;; (lsp-face-semhl-variable :background 

					(tree-sitter-hl-face:type :foreground ,fg2 :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:type.parameter :foreground ,other-green :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:type.argument :foreground ,other-blue :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:type.builtin :foreground ,pink-cat-boo-buffy-purple :slant italic :family "Liga OperatorMono Nerd Font")
					(tree-sitter-hl-face:type.super :foreground ,other-blue :weight Light :slant italic :family "Liga OperatorMono Nerd Font")
					
					(tree-sitter-hl-face:keyword :foreground ,pink-cat-boo-buffy-pink)
					(tree-sitter-hl-face:escape :foreground ,pink-cat-boo-buffy-pink)
					(tree-sitter-hl-face:label :foreground ,fg2)
					(tree-sitter-hl-face:constant :foreground ,fg4)
					(tree-sitter-hl-face:constant.builtin :foreground ,pink-cat-boo-buffy-cyan)
					(tree-sitter-hl-face:number :foreground ,pink-cat-boo-buffy-cyan)

					(lsp-headerline-breadcrumb-symbols-face :height 88 :foreground ,fg4)
					;; (lsp-headerline-breadcrumb-symbols-face :height 65)
					(lsp-headerline-breadcrumb-prefix-face :height 66 :foreground ,fg4)

					(org-block :background ,pink-cat-boo-buffy-dark :extend t)
					;; (org-code :foreground ,fg2 :background ,pink-cat-boo-buffy-dark)
					(org-code :background ,pink-cat-boo-buffy-dark :foreground ,fg4)
					(org-block-begin-line :box (:style released-button) :foreground ,fg4 :height 62 :weight bold)
					(org-block-end-line :box (:style released-button) :foreground ,fg4 :height 62 :weight bold)
					;; (org-block-end-line :background ,bg3)

					(sp-show-pair-match-content-face :background ,"#351628")
					(sp-wrap-overlay-face :background ,pink-cat-boo-buffy-dark)
					(sp-wrap-tag-overlay-face :background ,pink-cat-boo-buffy-dark)
					(sp-wrap-overlay-closing-pair :background ,pink-cat-boo-buffy-dark)
					(sp-wrap-overlay-opening-pair :background ,pink-cat-boo-buffy-dark :foreground ,fg2 :weight bold)
					
					;; (highlight :background ,pink-cat-boo-buffy-dark :foreground ,fg4)
					;; (highlight :background ,pink-cat-boo-buffy-dark)
					)))

  (apply #'custom-theme-set-faces
         'pink-cat-boo-buffy
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (term-colors (mapcar #'car (mapcar #'cddr colors)))
               (tty-colors (mapcar #'car (mapcar #'last colors)))
               (expand-for-kind
                (lambda (kind spec)
                  (when (and (string= (symbol-name kind) "term-colors")
                             pink-cat-boo-buffy-use-24-bit-colors-on-256-colors-terms)
                    (setq kind 'graphic-colors))
                  (cl-progv color-names (symbol-value kind)
                    (eval `(backquote ,spec))))))
           (cl-loop for (face . spec) in faces
                    collect `(,face
                              ((((min-colors 16777216)) ; fully graphical envs
                                ,(funcall expand-for-kind 'graphic-colors spec))
                               (((min-colors 256))	; terminal withs 256 colors
                                ,(funcall expand-for-kind 'term-colors spec))
                               (t		 ; should be only tty-like envs
                                ,(funcall expand-for-kind 'tty-colors spec))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pink-cat-boo-buffy)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; pink-cat-boo-buffy-theme.el ends here
