;; Package management configurations.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package)
  )
;; TODO: Use Diminish for RYO
													 ;(require 'diminish)
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Persient Soft
(straight-use-package 'pcache)
(straight-use-package 'persistent-soft)
(require 'persistent-soft)
													 ;(persistent-soft-store 'hundred 100 "mydatastore")
(persistent-soft-fetch 'hundred "mydatastore")
													 ;(persistent-soft-fetch 'thousand "mydatastore")

;; Vertical completion.
(straight-use-package 'selectrum)
(selectrum-mode t)
(straight-use-package 'prescient)
;; (prescient-filter-method fuzzy)

;; (straight-use-package 'company-prescient)
;; (company-prescient-mode t)

(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode t)
(prescient-persist-mode t)

(straight-use-package 'treemacs)

;; Languages
;; V
(straight-use-package
 '(vlang-mode :type git :host github :repo "Naheel-Azawy/vlang-mode")
 )

;; Completion
(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(straight-use-package 'company)
(straight-use-package 'company-posframe)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'company-box)
(add-hook 'company-mode 'company-box-mode)

(use-package lsp-mode
  :straight t
  :hook (lisp-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  ;; :commands lsp
  )
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  )
(add-to-list 'load-path "~/.emacs.d/cask/core/")
(add-to-list 'load-path "~/.emacs.d/cask/lisp/")
(add-to-list 'load-path "~/.emacs.d/cask/langs/")
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list
  )

(straight-use-package 'dap-mode)

;; Searching
;; TODO: Configure this
(straight-use-package 'ctrlf)
(ctrlf-mode t)

(straight-use-package 'restart-emacs)

;; EDITOR SETTINGS
(when (display-graphic-p) ; Start full screen
  (add-to-list 'default-frame-alist '(fullscreen . t))
  (x-focus-frame nil)
  )
(setq inhibit-splash-screen t)

;; Make frame borderless.
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

(setq-default frame-title-format "%b (%f)")
(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            )
		)

(use-package hungry-delete
  :straight t
  :config
  (global-hungry-delete-mode)
  )

(straight-use-package 'syntax-subword)
(global-syntax-subword-mode t)

;; Ripgrep
													 ;(grep-apply-setting
													 ;   'grep-find-command
													 ;   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)
													 ;	)

(setq-default
 tab-width 3
 electric-pair-mode t
 )

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default require-final-newline t)
(add-hook 'org-mode-hook (lambda () (require-final-newline nil)))

(global-visual-line-mode)

(use-package super-save
  :straight t
  :config
  (super-save-mode t)
  )

(use-package embark
  :straight (:host github :repo "oantolin/embark"
						 :branch "master")
  )

;; MODELINE
(straight-use-package 'all-the-icons)
(use-package powerline
  :straight t
  :config
  (powerline-center-theme))
(setq powerline-default-separator 'wave)

(use-package spaceline
  :straight t
  :demand t
  :config
  (require 'spaceline-config)
  )

(use-package spaceline-all-the-icons
  :straight t
  :after spaceline
  :defer t
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead)
  (custom-set-faces '(spaceline-highlight-face ((t (:background "#cb619e"
                                                                :foreground "#f8f8f2"
                                                                :inherit 'mode-line))))
                    '(powerline-active2 ((t (:background "#44475a"
                                                         :foregound "#50fa7b"
                                                         :inherit 'mode-line))))
                    '(mode-line ((t (:background "#282a36"
                                                 :foregound "#50fa7b"
                                                 :inherit 'mode-line))))
                    '(powerline-active1 ((t (:background "#6272a4"
                                                         :foregound "#50fa7b"
                                                         :inherit 'mode-line))))
						  )
  )
(setq spaceline-all-the-icons-separator-type 'wave)

(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; AESTHETICS
(add-to-list 'custom-theme-load-path (expand-file-name "./dracula-pro-theme/" user-emacs-directory))
(load-theme 'dracula-pro t)
(use-package dracula-pro-theme
  :defer 3
  :load-path "~/wgooch/.emacs.d/dracula-pro-theme"
  :init
  (load-theme 'dracula-pro t)
  )
(setq custom-safe-themes t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(defun graphic-p ()
  (display-graphic-p))
(when (graphic-p)
  ;;  transparent frame
  (add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist'(alpha . (95 . 89)))
  )

;; Font
(add-hook 'prog-mode 'font-lock-mode)
(set-face-attribute 'default nil
						  :family "Fira Code"
						  :weight 'SemiBold
						  :height 90
						  )

(set-face-attribute 'font-lock-comment-face nil
						  :family "Liga OperatorMono Nerd Font"
						  :weight 'Light
						  :height 90
						  :slant 'italic
						  )
(set-face-attribute 'font-lock-keyword-face nil
						  :family "Fira Code"
						  :weight 'Bold
						  )

(set-face-attribute 'variable-pitch nil
						  :family "Input Serif Compressed"
						  :weight 'Medium
						  :height 100
						  )

(set-face-attribute 'fixed-pitch nil
						  :family "Fira Code"
						  :weight 'SemiBold
						  :height 90
						  )

;; Change font size by monitor.
(defun fontify-frame ()
  (interactive)
  ;; (if window-system
  (progn
	 ;; (if (eq (x-display-pixel-width) 1920)
    (if (equal 'x-display-pixel-width 1920)
	 ;; (if (eq x-display-name "172.22.192.1:0.0")
	 ;; (if (eq (display-monitor-attributes-list . )
		  (progn
			 ;; Laptop
          (set-frame-parameter (selected-frame) 'font "Fira Code 10")
			 (message "On Laptop")
			 )
		(progn
		  (if (eq (x-display-pixel-width) 1050)
				(progn
				  ;; Monitor
				  (set-frame-parameter (selected-frame) 'font "Fira Code 8")
				  (message "On Monitor")
				  )
			 (progn
				(message "Failed")
				)
			 )
		  )
		)
	 )
  ;; )
  )

;; Fontify current frame
;; (fontify-frame nil)

;; Fontify any future frames
;; (push 'fontify-frame after-make-frame-functions)
;; (add-hook 'window-size-change-functions #'fontify-frame)

													 ; |-----  -|- ----|
													 ; |--------|-----|
													 ; |-|-

;; The quick brown fox jumped over the lazy dog
;; The quick brown fox jumped over the lazy dog.
;; The quick brown fox jumped over the lazy dog.

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el"
						 :branch "master")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "ligature" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|>" "{|" "|-" "-|" "-|-"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\" "://" "||-"))
  (ligature-set-ligatures 'text-mode '("|-" "-|-" "-|" "---"))
  (global-ligature-mode t)
  )
(add-to-list 'ligature-composition-table `(text-mode ("=" . ,(rx (+ "=")))))

;; This takes too long to set up. Not needed yet.
;; (straight-use-package 'unicode-fonts)
;; (unicode-fonts-setup)

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; Highlight current line
;; (use-package hl-line+
;; :load-path "./random-libs"
;; :init
;; (setq hl-line-overlay-priority 100)
;; (set-face-attribute hl-line-face nil :box t)
;; (set-face-foreground 'hl-line "#9580FF")
;; (set-face-underline-p 'highlight t)
;; (toggle-hl-line-when-idle t)
;; (hl-line-when-idle-interval 10)
;; )
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)

;; Line Numbers
(use-package linum-relative
  :straight t
  :init
  ;; This backend is faster, but its faces do not work.
  (setq linum-relative-backend 'display-line-numbers-mode)
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode t)
  )

(use-package yascroll
  :straight t
  )
(setq yascroll:delay-to-hide 9.0)
(global-yascroll-bar-mode t)
(setq scroll-preserve-screen-position t)

(use-package sublimity
  :straight t
  :init
  (sublimity-mode t)
  )
(require 'sublimity-scroll)
(setq sublimity-scroll-weight 12
		sublimity-scroll-drift-length 2
		)

(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t nil)))
 '(custom-comment ((t (:background "dim gray" :family "Operator Mono Lig Medium"))))
 '(hydra-face-red ((t (:foreground "violet")))))


;; DIRED
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'ryo-enable)
(straight-use-package 'posframe)

;; Load posframe locally.
(native-compile-async "~/.emacs.d/dired-posframe.el/" 'recursively)
(add-to-list 'load-path "~/.emacs.d/dired-posframe.el/")
(load "dired-posframe")

;; I've submitted a pull request adding this. If it's accepted, I'll uncomment.
;; (use-package dired-posframe
;; :straight t
;; :config
;; (add-to-list 'dired-posframe-advice-alist
;; '(
;; (dired-next-dirline . dired-posframe--advice-show)
;; (dired-prev-dirline . dired-posframe--advice-show)
;; (dired-omit-mode . dired-posframe--advice-show)
;; )
;; )
;; )

(straight-use-package 'dired-git)
(add-hook 'dired-mode-hook 'dired-git-mode)
(setq initial-buffer-choice "~/.emacs.d/")
(add-hook 'emacs-startup-hook #'dired-jump)

;; TODO: Replace this with RYO bindings.
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

(setq-default dired-omit-files-p t)
(setq dired-omit-verbose nil)

;; Custom utility scripts.
;; There exist states, such as after kill-region, when the cursor color is incorrect.
(defun ryo-cursor-update ()
  (if ryo-modal-mode
		(progn
		  (setq cursor-type (cons 'box 2))
		  (message "NORMAL MODE")

		  (if (region-active-p)
				(keyboard-quit)
			 )
		  )
	 (progn
		(setq cursor-type (cons 'bar 2))
		(message "INSERT MODE")
		)
	 )
  )

(defun pony-toggle-mark ()
  (interactive)
  (if (region-active-p)
		(progn
		  (setq cursor-type 'box)
		  (keyboard-quit)
		  (message "Cancel Selection")   ; This is not working.
		  )
	 (progn
		(set-mark (point))
		(setq cursor-type 'hbox)
		(message "Selecting")
		)
	 )
  )

(defun dired-posframe-toggle ()
  (interactive)
  (if (bound-and-true-p dired-posframe-mode)
		(progn
		  (setq dired-posframe-mode nil)
		  (dired-posframe-teardown)
		  (message "Posframe Disabled")
		  )
	 (progn
		(setq dired-posframe-mode t)
		(dired-posframe-setup)
		(dired-posframe-show)
		(message "Posframe Enabled")
		)
	 )
  )

(defun dired-alternate-up ()
  (interactive)
  (find-alternate-file "..")
  )

(defun eval-dwim ()
  (interactive)
  (if (region-active-p)
		(eval-region (region-beginning) (region-end))
	 (eval-buffer)
	 )
  )


;; ORG
(use-package org
  :straight t
  :config
  (setq-default
	org-src-fontify-natively t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-pretty-entities-include-sub-superscripts t
  )
  ;; (setq org-hide-leading-stars t)
  )
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(use-package org-bullets
  :straight t
  :init
  ;; (setq org-bullets-face-name "Inconsolata-12")
  (setq org-bullets-bullet-list
        '("◎" "⚫" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
  )

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(eval-after-load "org"
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block 'org-table)))

;; Make Bibtex export in PDF
(setq org-latex-pdf-process
		'("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(setq org-todo-keyword-faces
      '(
		  ("NOTE" . (:foreground "lightgreen" :weight bold))
		  ("LOW" . (:foreground "#cb4b16" :background "snow" :weight bold :box (:line-width -1)))
        ("MED" . (:foreground "goldenrod" :background "cornsilk" :weight bold :box (:line-width -1)))
        ("HIGH" . (:foreground "darkgreen" :background "honeydew" :weight bold :box (:line-width -1)))
		  ("TODO" . (:foreground "black" :background "snow" :weight bold :box (:line-width -1)))
        ("DONE" . org-done)
        ("WONTFIX" . org-done)
		  )
		)

(executable-find "sqlite3")
(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/path/to/org-files/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  )


;; WHICH KEY
(use-package which-key
  :straight t
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  )
(push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
(setq which-key-idle-delay 0.1)
(setq which-key-prefix-prefix "")
(setq which-key-show-prefix 'bottom)
(setq which-key-show-transient-maps t)
(which-key-mode)

;; Buffer management
;; (straight-use-package 'persp-mode)
(straight-use-package 'ace-window)
(straight-use-package 'buffer-move)

;; GIT
(straight-use-package 'magit)

(straight-use-package 'vc-msg)
(eval-after-load 'vc-msg-git
  '(progn
     ;; show code of commit
     (setq vc-msg-git-show-commit-function 'magit-show-commit)
     ;; open file of certain revision
     (push '("m"
             "[m]agit-find-file"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info)
                      (git-dir (locate-dominating-file default-directory ".git")))
                 (magit-find-file (plist-get info :id )
                                  (concat git-dir (plist-get info :filename))))))
           vc-msg-git-extra))
  )

(use-package git-gutter
  :straight t
  )
(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'org-mode-hook 'git-gutter-mode)
(custom-set-variables
 '(git-gutter:update-interval 2)
 '(git-gutter:window-width 1)
 '(git-gutter:modified-sign " ")
 '(git-gutter:added-sign "|")
 '(git-gutter:deleted-sign " ")
 )
(set-face-foreground 'git-gutter:added "orange")

;; YASnippet
(use-package yasnippet
  :straight t
  :init
  )
(yas-global-mode t)
(straight-use-package 'yasnippet-snippets)

;; PONY FLY KEYS
(native-compile-async "~/.emacs.d/Pony-Fly-Keys/" 'recursively)
(add-to-list 'load-path "~/.emacs.d/Pony-Fly-Keys/")
(load "pony-fly-keys")

;; Toggle mark state.
(straight-use-package 'selected)
(selected-global-mode t)

;; This seems redundant.
(add-hook 'ryo-modal-mode-hook 'ryo-cursor-update)

;; I'm using some of Xah's utilities rn.
;; (straight-use-package 'xah-fly-keys)
(use-package xah-fly-keys
  :straight t
  )

;; Hydra
(straight-use-package 'hydra)

;; Chords
(straight-use-package 'key-chord)
(key-chord-define selectrum-minibuffer-map "ut" 'selectrum-next-candidate)
(key-chord-define selectrum-minibuffer-map "uc" 'selectrum-previous-candidate)
(key-chord-define selectrum-minibuffer-map "us" 'selectrum-next-page)
(key-chord-define selectrum-minibuffer-map "ud" 'selectrum-previous-page)
(key-chord-mode t)

;; Multi Cursors
(straight-use-package 'multiple-cursors)
(straight-use-package 'mc-calc)

;; Bookmarks
(setq bm-restore-repository-on-load t)
(use-package bm
  :straight t
  :demand t
  :init
  (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
  (autoload 'bm-next     "bm" "Goto bookmark."                     t)
  (autoload 'bm-previous "bm" "Goto previous bookmark."            t)
  :config
  ;; Save bookmarks
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq bm-buffer-persistence t)
  ;; Optional labels
  (setq bm-annotate-on-create t)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)
										  ))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  ;; Aesthetics
  ;; Currently, fringe-bitmap 'bm-marker is non-configable.
  (setq bm-marker 'bm-marker-left
        bm-recenter t
        bm-highlight-style 'bm-highlight-only-fringe
		  )
  ;; These are not working.
  (add-hook 'bm-show-mode-hook #'ryo-enable)
  (add-hook 'bm-show-mode-hook #'hl-line-mode)
  )
(add-hook 'bookmark-bmenu-mode-hook 'ryo-enable)

;; MODAL EDITING
(setq-default cursor-type (cons 'bar 2))
(defun ryo-enable ()
  (interactive)
  (unless ryo-modal-mode
	 (ryo-modal-mode)
	 )
  (ryo-cursor-update)
  (selected-off)
  )

;; Keybindings.
(use-package ryo-modal
  :straight t
  :commands ryo-modal-mode
  :bind ("M-SPC" . ryo-modal-mode)
  :init
  (global-set-key [backspace] 'ryo-enable)
  :config
  (setq ryo-modal-cursor-type t)
  (setq ryo-modal-cursor-color nil)

  (ryo-modal-keys
   (:exit t)
   ("u" ryo-modal-mode :name "Insert Mode")
	)

  (ryo-modal-keys
	("a" execute-extended-command)
	;; Basic navigation controls.
   ("t" next-logical-line)
   ("T" next-line)
	("C-t" selectrum-next-candidate)
	("c" previous-logical-line)
   ("C" previous-line)
	("C-c" selectrum-previous-candidate)
	("g" backward-char)
   ("r" forward-char)
   ("h" pony-move-left-word)
	("H" syntax-subword-left)
	("n" pony-move-right-word)
	("N" syntax-subword-right)
	("d" xah-beginning-of-line-or-block)
	("s" xah-end-of-line-or-block)
	("D" pony-binary-beginning-of-line)
	("S" pony-binary-end-of-line)

	;; Basic deletion commands.
	("e" delete-backward-char)
	("(" delete-char)
	("." pony-delete-left-word)
	(">" subword-backward-kill)
	("p" pony-delete-right-word)
	("P" subword-kill)

	;; Commenting
	("'" comment-line)

	;; Selection
	("*" pony-mark-line)
	(")" mark-word)
	("+" xah-select-block)
	("!" rectangle-mark-mode)

	;; Multi-cursor
	("C" mc/mark-previous-like-this)
	("T" mc/mark-next-like-this)

	;; Bookmarks
	("i" (
			;; TODO: Configure bookmarks.
			("c" bm-previous :name "Bookmark Up")
			("t" bm-next :name "Bookmark Down")
			("h" bm-toggle :name "Bookmark Toggle")
			("." bm-show :name "Bookmark Jump")
			("e" bm-remove-all-current-buffer "Remove All")
			)
	 )
	)

  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")

	;; Basic operations.
	("f" undo)
	("F" undo-redo)
	("q" kill-region)
	("j" pony-copy-current-word)
	("k" yank)
	("y" pony-toggle-mark)
	("b" ctrlf-forward-fuzzy)
	("B" ctrlf-forward-literal)
	("," xah-shrink-whitespaces)
	)

  ;; Buffer management hydra
  (ryo-modal-keys
	;; ("SPC" (
	;; ("SPC g" :hydra
	("w" :hydra
		  ;; ("SPC g" :hydra
		  '(hydra-buffer (:color red)
			 "Buffer Hydra"
			 ("t" split-window-below "Split Vertically")
			 ("n" split-window-right "Split Rightward")
			 ("M-t" buf-move-down "Swap Down")
			 ("M-c" buf-move-up "Swap Up")
			 ("M-n" buf-move-right "Swap Right")
			 ("M-h" buf-move-left "Swap Left")
			 ("T" windmove-down "Focus Down")
			 ("C" windmove-up "Focus Up")
			 ("H" windmove-left "Focus Left")
			 ("N" windmove-right "Focus Right")
			 ("e" delete-window "Kill")
			 ("(" ace-delete-window "Kill Other")
			 ("w" ace-delete-other-windows "Kill All Except")
			 ("p" switch-to-buffer "Switch")

			 ;; Resizing
			 (">" enlarge-window :name "Grow Vert")
			 ("E" shrink-window :name "Shrink Vert")
			 ("U" enlarge-window-horizontally :name "Grow Hor")
			 ("O" shrink-window-horizontally :name "Shrink Hor")
			 ("," balance-windows)
			 ;; TODO: Configure IBuffer commands.
			 ("." ibuffer "IBuffer")
			 ("<BACKSPACE>" nil "Cancel" :color blue)
			 )
		  )

	;; Multi-cursor bindings
	;; TODO: This would be better as
	;; "!" when a selection exists.
	("-" (
			("e" mc/edit-lines)
			("d" mc/edit-beginnings-of-lines)
			("s" mc/edit-ends-of-lines)
			("+" mc/mark-all-in-region)
			(")" mc/mark-all-dwim)
			)
	 )
	)

  (ryo-modal-keys
	;; Leader key
	("SPC" (
			  ("a" mark-whole-buffer "Select All")
			  ("b" exchange-point-and-mark "Exchange Point and Mark")
			  ("d" beginning-of-buffer "Buffer Start")
			  ("s" end-of-buffer "Buffer End")

			  ;; Inserts
			  ("e" (
					  ("e" yas-insert-snippet :name "Snippet")
					  ("h" xah-insert-brace :name "{}")
					  ("t" xah-insert-paren :name "()")
					  ("n" xah-insert-square-bracket :name "[]")
					  ("g" xah-insert-ascii-double-quote :name "\"\"")
					  ("c" xah-insert-ascii-double-quote :name "\'\'")
					  ("r" pony-insert-region-pair :name "Region")
					  ("!" (
							  ("t" mc/insert-numbers)
							  ("h" mc/insert-letters)
							  ("n" mc/sort-regions)
							  ("s" mc/reverse-regions)
							  )
						:name "Multi-Cursor"
						)
					  )
				:name "Insert"
				)

			  ;; File Management
			  ("c" (
                 ("s" save-buffer :name "Save Buffer")
                 ("o" write-file :name "Save As")
                 ("," dired :name "Dired Path")
					  ("." dired-jump :name "Dired")
                 ("a" magit-status :name "Git")
					  (";" xah-show-in-desktop :name "Explorer")
					  ;; ("h" recentf-open-files :name "Open Recent")
					  ("n" xah-new-empty-buffer :name "New File")
					  ("p" xah-open-last-closed :name "Open Last Closed")
					  ("f" xah-open-recently-closed :name "Open Recent Closed")
					  ("d" eval-dwim :name "Evaluate")
                 )
            :name "File"
            )

			  ;; Large Motions
           ("t" (
                 ("n" scroll-up :name "Page Down")
                 ("N" zz-scroll-half-page-down :name "Half-Page Down")
                 ;; g can be used for scroll without moving cursor
                 ("h" scroll-down :name "Page Up")
                 ("H" zz-scroll-half-page-up :name "Half-Page Down")
                 ;; r can be used for scroll without moving cursor
                 ("u" recenter-top-bottom :name "Recenter Point")
                 ("e" move-to-window-line-top-bottom :name "Point at Center")
					  ("b" kill-this-buffer :name "Kill This Buffer")
                 )
            :name "Large Motion"
            )
           )
    :name "LEADER"
    )
   )

  (ryo-modal-keys
	;; Dired Mode
	(:norepeat t :mode 'dired-mode)
	("j" dired-do-copy)
	("." dired-alternate-up)
	("y" dired-mark)
	("Y" dired-unmark)
	("c" dired-previous-line)
	("t" dired-next-line)
	("n" dired-next-dirline)
	("h" dired-prev-dirline)
	("b" find-name-dired)
	("'" dired-omit-mode :name "Hide Details")
	("e" dired-do-delete)
	("SPC" (
			  ("i" dired-do-rename)
			  ("," dired-do-compress-to)
			  ("<" dired-do-compress)
			  ("y" dired-unmark-all-marks :name "Unmark All")
			  ("." dired-posframe-toggle)
			  ("'" dired-mark-omitted :name "Omit")
			  ("c" (
					  ("n" find-file :name "Create File")
					  ("t" dired-create-directory :name "Create Directory")
			 		  )
				)
			  ("t" (
					  ("." dired-do-load :name "Load Elisp")
					  ("b" quit-window :name "Close Dired")
					  )
				)
			  ("b" (
					  ("." dired-mark-extension)
					  ("b" dired-do-find-regexp)
					  )
				;; :name "Finding"
				)
			  )
	 )
	)

  (ryo-modal-keys
	;; Org Mode
	(:mode 'org-mode)
	("'" (
			("u" org-table-create)
			)
	 :name "Table"
	 )

	("SPC" (
			  ("n" org-do-demote)
			  ("h" org-do-promote)
			  ("TAB" org-todo :name "Toggle TODO")
			  ("u" (
					  ("n" org-insert-todo-subheading)
					  ("t" org-insert-todo-heading)
					  )
				)
			  ("e" (
					  ("n" org-insert-subheading)
					  ("t" org-insert-heading)
					  )
				)
			  ("SPC" (
						 ("TAB" org-toggle-narrow-to-subtree)
						 ("t" org-next-visible-heading)
						 ("c" org-previous-visible-heading)
						 ("T" org-forward-heading-same-level)
						 ("C" org-backward-heading-same-level)
						 ("h" outline-up-heading)
						 ("n" org-goto)
						 )
				:name "Headings"
				)
			  )
	 )
	)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	'("89ba918121c69681960ac1e4397296b5a756b1293325cee0cb543d70418bd556" "bcb58b7e1a372e677f44e25e3da88f283090dbd506550c137d02907446c7d11c" "7451f243a18b4b37cabfec57facc01bd1fe28b00e101e488c61e1eed913d9db9" default))
 '(line-number-mode nil)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp))))
