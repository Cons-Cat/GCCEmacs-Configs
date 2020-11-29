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

;; Projectile
(straight-use-package 'projectile)
(projectile-mode t)

;; Syntax highlighting
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; Completion
(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package company
  :straight t
  :diminish
  :hook (company-mode . company-box-mode)
  :config
  (add-to-list 'company-backends '(company-files company-dabbrev))
   :custom
  (company-begin-commands '(self-insert-command))
  (company-require-match nil)
  (company-idle-delay .6)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-idle-delay 0)
  (company-dabbrev-downcase nil)
  (global-company-mode t)
  )
(add-hook 'prog-mode-hook 'company-mode)

(straight-use-package 'company-posframe)
(straight-use-package 'company-box)

(use-package lsp-mode
  :straight t
  :hook
  (lisp-mode . lsp-deferred)
  (c-mode-hook . lsp-deferred)
  (c++-mode-hook . lsp-deferred)
  :config
  (add-to-list 'company-backends '(company-lsp company-dabbrev))
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
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
  :config
  (add-to-list 'company-backends '(company-lsp company-dabbrev))
  )

(use-package company-tabnine
  :straight t
  :config
  (add-to-list 'company-backends #'company-tabnine)
  )

(straight-use-package 'flx)
(use-package company-fuzzy
  :straight t
  :config
  (add-to-list 'company-fuzzy-history-backends 'company-yasnippet)
  :custom
  (company-fuzzy-sorting-backend 'flx)
  (company-fuzzy-prefix-on-top t)
  (company-fuzzy-show-annotation t)
  )


;; Company keybindings
(add-hook 'company-mode-hook 'company-tng-mode)
(define-key company-active-map (kbd "TAB") 'company-select-next)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-active-map (kbd "RET") nil)


;; Debugging
;; TODO: Configure debugger.
(straight-use-package 'dap-mode)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))
			 )


;; Languages
;; V
(straight-use-package
 '(vlang-mode :type git :host github :repo "Naheel-Azawy/vlang-mode")
 )

;; C++
(add-hook 'c-mode-common-hook 'tree-sitter-mode)
;; (add-hook 'c-mode-common-hook #'lsp)

(use-package company-irony
  :straight t
  :init
  (add-to-list 'company-backends 'company-irony)
  )
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1 ;; clangd is fast
      ;; be more ide-ish
      lsp-headerline-breadcrumb-enable t
		)

(straight-use-package 'cmake-ide)

;; MySQL
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'emacsql)
(straight-use-package 'emacsql-mysql)

;;
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  )


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

;; This is disabled because it causes Avy to be slow.
;; This is a known Avy issue. I very much wish I'll be able to
;; enable it in all modes one day.
;; (global-visual-line-mode)
(add-hook 'text-mode 'visual-line-mode)

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
(put 'dired-find-alternate-file 'disabled nil)

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


;; Avy
(straight-use-package 'avy)


;; Word Rotation
(use-package grugru
  :straight t
  :init
  (grugru-default-setup)
  )


;; Spell Checking

;; (use-package wucuo
;;   :straight t
;;   :config
;;   (setq wucuo-flyspell-start-mode "fast")
;;   (setq wucuo-spell-check-buffer-predicate
;;         (lambda ()
;; 			 (not (memq major-mode
;; 							'(dired-mode
;;                        log-edit-mode
;;                        compilation-mode
;;                        help-mode
;;                        profiler-report-mode
;;                        speedbar-mode
;;                        gud-mode
;;                        calc-mode
;;                        Info-mode))))
;; 		  )
;;   )

(use-package flyspell-correct
  :straight t
  ;; :bind (:map flyspell-mode-map ("M-c" . flyspell-correct-wrapper))
  )

(use-package flyspell-correct-popup
  :straight t
  :defer t
  ;; :after flyspell-correct
  )

(use-package flyspell-correct-avy-menu
  :straight t
  :after flyspell-correct
  )
;; (defun flyspell-correct-popup-inline-actions (candidates word)

;; (add-hook 'prog-mode-hook #'wucuo-start)
;; (add-hook 'text-mode-hook #'wucuo-start)
;; (setq ispell-program-name "enchant-2")
;; (setq ispell-extra-args '("-a"))
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
;; (setq ispell-extra-args "--run-together")
;; emacs -batch -Q -l ~/projs/wucuo/wucuo.el --eval '(let* ((ispell-program-name "aspell") (ispell-extra-args (wucuo-aspell-cli-args t))) (wucuo-spell-check-directory "."))'

(use-package flyspell-lazy
  :straight t
  )
(add-hook 'flyspell-mode-hook 'flyspell-lazy-mode)
(add-to-list 'ispell-extra-args "--sug-mode=ultra")

;; TODO: Configure this for Org or comments or something.
;; https://laclefyoshi.hatenablog.com/entry/20150912/langtool_popup
;; (use-package langtool
;;   :straight t
;;   :config
;;   (setq langtool-default-language "en-US")
;;   )


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
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  )

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

;; Org Babel
;; (org-babel-do-load-languages
 ;; 'org-babel-load-languages
 ;; '((mysql . t))
 ;; )

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

(use-package company-org-roam
  ;; :when (featurep! :completion company)
  :straight t
  :after org-roam
  :config
  ;; (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev))
  )
(add-to-list 'company-backends '(company-org-roam))

;; (use-package org-journal
;;   :straight t
;;   :config
;;   (setq org-journal-enable-agenda-integration t)
;;   :custom
;;   (org-journal-dir "~/Desktop/03-resources/org-roam/")
;;   (org-journal-date-prefix "#+TITLE: ")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-date-format "%A, %d %B %Y")
;;   )

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


;; Snippets
(use-package yasnippet
  :straight t
  :init
  ;; (define-key yas-minor-mode-map "TAB" nil)
  ;; (define-key yas-minor-mode-map "C-RET" 'yas-next-field-or-maybe-expand)
  )

(setq company-lsp-enable-snippet nil)
(advice-add 'company-tng--supress-post-completion :override #'ignore)

(define-key yas-minor-mode-map "\M-t" 'yas-expand)
(define-key yas-keymap "\M-t" 'yas-next-field-or-maybe-expand)

(dolist (keymap (list yas-minor-mode-map yas-keymap))
  (define-key keymap (kbd "TAB") nil)
  (define-key keymap [(tab)] nil)
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
  :bind
  ("M-SPC" . ryo-modal-mode)
  ("M-TAB" . ryo-modal-mode)
  ;; :init
  ;; (global-set-key [escape] 'ryo-enable)
  :config
  (setq ryo-modal-cursor-type t)
  (setq ryo-modal-cursor-color nil)

  (ryo-modal-keys
   (:exit t)
   ("t" ryo-modal-mode :name "Insert Mode")
	)

  (ryo-modal-keys
	("r" execute-extended-command)
	;; Basic navigation controls.
   ("e" next-logical-line)
   ("E" next-line)
	;; ("C-t" selectrum-next-candidate)
	("o" previous-logical-line)
   ("O" previous-line)
	;; ("C-c" selectrum-previous-candidate)
	("c" backward-char)
   ("j" forward-char)
   ("u" pony-move-left-word)
	("U" syntax-subword-left)
	("a" pony-move-right-word)
	("A" syntax-subword-right)
	("y" xah-beginning-of-line-or-block)
	("i" xah-end-of-line-or-block)
	("Y" pony-binary-beginning-of-line)
	("I" pony-binary-end-of-line)

	;; Basic deletion commands.
	("n" delete-backward-char)
	("=" delete-char)
	("h" pony-delete-left-word)
	("H" subword-backward-kill)
	("k" pony-delete-right-word)
	("K" subword-kill)

	;; Commenting
	("x" comment-line)

	;; Selection
	("0" pony-mark-line)
	("(" mark-word)
	("{" xah-select-block)
	("[" rectangle-mark-mode)

	;; Multi-cursor
	("M-o" mc/mark-previous-like-this)
	("M-e" mc/mark-next-like-this)

	;; Bookmarks
	;; ("p" (
			;; TODO: Configure bookmarks.
			;; ("e" bm-next :name "Bookmark Down")
			;; ("u" bm-toggle :name "Bookmark Toggle")
			;; ("h" bm-show :name "Bookmark Jump")
			;; ("N" bm-remove-all-current-buffer "Remove All")
			;; )
	 ;; )
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
	("m" undo)
	("M" undo-redo)
	("b" kill-region)
	("l" pony-copy-current-word)
	("d" yank)
	("v" pony-toggle-mark)
	("w" ctrlf-forward-fuzzy)
	("W" ctrlf-forward-literal)
	("g" xah-shrink-whitespaces)
	("2" grugru)
	)

  ;; Buffer management hydra
  (ryo-modal-keys
	;; ("SPC" (
	;; ("SPC g" :hydra
	("q" :hydra
		  ;; ("SPC g" :hydra
		  '(hydra-buffer (:color red)
			 "Buffer Hydra"
			 ("e" split-window-below "Split Vertically")
			 ("a" split-window-right "Split Rightward")
			 ("M-e" buf-move-down "Swap Down")
			 ("M-o" buf-move-up "Swap Up")
			 ("M-a" buf-move-right "Swap Right")
			 ("M-u" buf-move-left "Swap Left")
			 ("E" windmove-down "Focus Down")
			 ("O" windmove-up "Focus Up")
			 ("U" windmove-left "Focus Left")
			 ("A" windmove-right "Focus Right")
			 ("n" delete-window "Kill")
			 ("=" ace-delete-window "Kill Other")
			 ("q" ace-delete-other-windows "Kill All Except")
			 ("k" switch-to-buffer "Switch")

			 ;; Resizing
			 ("H" enlarge-window :name "Grow Vert")
			 ("N" shrink-window :name "Shrink Vert")
			 ("T" enlarge-window-horizontally :name "Grow Hor")
			 ("S" shrink-window-horizontally :name "Shrink Hor")
			 ("g" balance-windows)
			 ;; TODO: Configure IBuffer commands.
			 ("h" ibuffer "IBuffer")
			 ("<BACKSPACE>" nil "Cancel" :color blue)
			 )
		  )

	;; Multi-cursor bindings
	;; TODO: This would be better as
	;; "2" when a selection exists.
	("_" (
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
			  ("r" mark-whole-buffer "Select All")
			  ("w" exchange-point-and-mark "Exchange Point and Mark")
			  ("y" beginning-of-buffer "Buffer Start")
			  ("i" end-of-buffer "Buffer End")

			  ;; Inserts
			  ("n" (
					  ;; TODO: Consider company-yasnippet
					  ("e" yas-insert-snippet :name "Snippet")
					  ("h" xah-insert-brace :name "{}")
					  ("t" xah-insert-paren :name "()")
					  ("n" xah-insert-square-bracket :name "[]")
					  ("g" xah-insert-ascii-double-quote :name "\"\"")
					  ("c" xah-insert-ascii-double-quote :name "\'\'")
					  ("r" pony-insert-region-pair :name "Region")
					  ("2" (
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
			  ("o" (
                 ("i" save-buffer :name "Save Buffer")
                 ("s" write-file :name "Save As")
                 ("g" dired :name "Dired Path")
					  ("h" dired-jump :name "Dired")
                 ("r" magit-status :name "Git")
					  ("x" xah-show-in-desktop :name "Explorer")
					  ;; ("h" recentf-open-files :name "Open Recent")
					  ("a" xah-new-empty-buffer :name "New File")
					  ("m" xah-open-last-closed :name "Open Last Closed")
					  ("v" xah-open-recently-closed :name "Open Recent Closed")
					  ("y" eval-dwim :name "Evaluate")
                 )
            :name "File"
            )

			  ;; Large Motions
           ("e" (
                 ("a" scroll-up :name "Page Down")
                 ("A" zz-scroll-half-page-down :name "Half-Page Down")
                 ;; g can be used for scroll without moving cursor
                 ("u" scroll-down :name "Page Up")
                 ("U" zz-scroll-half-page-up :name "Half-Page Down")
                 ;; r can be used for scroll without moving cursor
                 ("n" recenter-top-bottom :name "Recenter Point")
                 ("t" move-to-window-line-top-bottom :name "Point at Center")
					  ("f" kill-this-buffer :name "Kill This Buffer")
					  ("h" avy-goto-line :name "Line Jump")
                 ("k" avy-goto-word-1 :name "Word Jump")
                 )
            :name "Large Motion"
            )

			  ;; Spellcheck
			  ("g" (
					  ("h" flyspell-correct-wrapper :name "Suggestions")
					  ("e" flyspell-correct-next :name "Next")
					  ("o" flyspell-correct-previous :name "Previous")
					  )
				:name "Spell Checking"
				)
           )
    :name "LEADER"
    )
   )

  (ryo-modal-keys
	;; Dired Mode
	(:norepeat t :mode 'dired-mode)
	("l" dired-do-copy)
	("h" dired-alternate-up)
	("m" dired-mark)
	("M" dired-unmark)
	("o" dired-previous-line)
	("e" dired-next-line)
	("a" dired-next-dirline)
	("u" dired-prev-dirline)
	("w" find-name-dired)
	("x" dired-omit-mode :name "Hide Details")
	("n" dired-do-delete)
	("SPC" (
			  ("p" dired-do-rename)
			  ("," dired-do-compress-to)
			  ("G" dired-do-compress)
			  ("m" dired-unmark-all-marks :name "Unmark All")
			  ("h" dired-posframe-toggle)
			  ("x" dired-mark-omitted :name "Omit")
			  ("o" (
					  ("a" find-file :name "Create File")
					  ("e" dired-create-directory :name "Create Directory")
			 		  )
				)
			  ("e" (
					  ("h" dired-do-load :name "Load Elisp")
					  ("w" quit-window :name "Close Dired")
					  )
				)
			  ("w" (
					  ("h" dired-mark-extension)
					  ("w" dired-do-find-regexp)
					  )
				;; :name "Finding"
				)
			  )
	 )
	)

  (ryo-modal-keys
	;; Org Mode
	(:mode 'org-mode)
	("x" (
			("t" org-table-create)
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
