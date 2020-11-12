
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

;; TODO: Install Embark
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode t)

(prescient-persist-mode t)

;; TODO: Configure this
(straight-use-package 'ctrlf)
(ctrlf-mode t)

(straight-use-package 'restart-emacs)


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
(straight-use-package 'dracula-theme)
(load-theme 'dracula t)
(setq custom-safe-themes t)

(hl-line-mode t)
;; 753190246
;; Font
(add-hook 'prog-mode 'font-lock-mode)
(set-face-attribute 'default nil
						  :family "Fira Code"
						  :weight 'SemiBold
						  :height 90
						  )
(set-face-attribute 'font-lock-comment-face nil
													 ;:family "Liga Dank Mono"
						  :family "Liga OperatorMono Nerd Font"
						  :weight 'Light
						  :height 90
						  :slant 'italic
						  )
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
  ;(www-ligature-mode t)
  (global-ligature-mode t)
  )
(add-to-list 'ligature-composition-table `(text-mode ("=" . ,(rx (+ "=")))))

(straight-use-package 'unicode-fonts)
(unicode-fonts-setup)

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(straight-use-package 'linum-relative)
(setq linum-relative-backend 'display-line-numbers-mode)
(setq linum-relative-current-symbol "")
(add-hook 'text-mode-hook 'linum-on)

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
(setq sublimity-scroll-weight 16
		sublimity-scroll-drift-length 2
		)

(setq x-stretch-cursor t)
(setq cursor-in-non-selected-windows nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "violet" :foreground "violet"))))
 '(custom-comment ((t (:background "dim gray" :family "Operator Mono Lig Medium"))))
 '(hydra-face-red ((t (:foreground "violet"))))
 )

;; FILE BROWSING
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(straight-use-package 'dired-posframe)
(add-hook 'dired-mode-hook 'dired-posframe-mode)
(straight-use-package 'dired-git)
(add-hook 'dired-mode-hook 'dired-git-mode)
(add-hook 'after-init-hook #'dired-jump)
													 ; Thanks, Xah!
;; TODO: Replace this with RYO bindings.
;(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

;(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory


;; EDITOR SETTINGS
(when (display-graphic-p) ; Start full screen
  (add-to-list 'default-frame-alist '(fullscreen . t))
  (x-focus-frame nil)
  )
(setq inhibit-splash-screen t)

(setq-default frame-title-format "%b (%f)")
(setq electric-pair-pairs '(
                           (?\{ . ?\})
                           (?\( . ?\))
                           (?\[ . ?\])
                           (?\" . ?\")
                           ))

(use-package hungry-delete
  :straight t
  :config
  (global-hungry-delete-mode)
  )

(setq-default
 tab-width 3
 electric-pair-mode t
 )

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

(global-visual-line-mode)

(use-package super-save
  :straight t
  :config
  (super-save-mode t)
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
(which-key-mode)


;; Buffer management
;; (straight-use-package 'persp-mode)
;; (straight-use-package 'ace-window)
(straight-use-package 'buffer-move)

;; GIT
(straight-use-package 'magit)


;; PONY FLY KEYS
(native-compile-async "~/.emacs.d/Pony-Fly-Keys/" 'recursively)
(add-to-list 'load-path "~/.emacs.d/Pony-Fly-Keys/")
(load "pony-fly-keys")

;; MODAL EDITING
(setq-default cursor-type (cons 'bar 2))
(defun ryo-enable ()
  (interactive)
  (unless ryo-modal-mode
	 (ryo-modal-mode)
	 )
  (ryo-cursor-update)
  )

;; There exist states, such as after kill-region, when the cursor color is incorrect.
(defun ryo-cursor-update ()
  (if ryo-modal-mode
		(progn
		  (setq cursor-type (cons 'box 1))
		  ;(set-cursor-color nil)
		  (message "NORMAL MODE")
		  )
	 (progn
		(setq cursor-type (cons 'bar 2))
		(set-cursor-color "darkmagenta")
		(message "INSERT MODE")
		)
	 )
  )

;; This seems redundant.
(add-hook 'ryo-modal-mode-hook 'ryo-cursor-update)

;; Hydra
(straight-use-package 'hydra)

;; Keybindings.
(use-package ryo-modal
    :straight t
    :commands ryo-modal-mode
    :bind ("M-SPC" . ryo-modal-mode)
    :init
    (global-set-key [backspace] 'ryo-enable) ;; Backspace
    :config
	 (setq ryo-modal-cursor-type t)
	 (setq ryo-modal-cursor-color "violet")
	 (ryo-modal-keys
     (:exit t)
     ("u" ryo-modal-mode :name "Insert Mode")
	)

    (ryo-modal-keys
	  ("a" execute-extended-command)
	  ;; Basic navigation controls.
     ("t" next-logical-line)
     ("T" next-line)
     ("c" previous-logical-line)
     ("C" previous-line)
	  ("g" backward-char)
     ("r" forward-char)
     ("h" pony-move-left-word)
	  ("n" pony-move-right-word)
	  ("d" beginning-of-line)
	  ("s" end-of-line)
	  ("D" pony-binary-beginning-of-line)
	  ("S" pony-binary-end-of-line)

	  ;; Basic deletion commands.
	  ("e" delete-backward-char)
	  ("(" delete-char)
	  ("." pony-delete-left-word)
	  ("p" pony-delete-right-word)

	  ;; Commenting
	  ("'" comment-line)

	  ;; Selection
	  ("*" pony-mark-line)
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

		  ;; Buffer management.
		  ("w" :hydra
			'(hydra-buffer ()
								"Buffer Hydra"
								("t" split-window-below "Split Vertically")
								("n" split-window-right "Split Rightward")

								("T" buf-move-down "Focus Down")
								("C" buf-move-up "Focus Up")
								("N" buf-move-right "Focus Right")
								("H" buf-move-left "Focus Left")

								("e" ace-delete-window "Kill")
								("w" ace-delete-other-windows "Kill All Except")
								("<BACKSPACE>" nil "Cancel" :color blue)
								)
			)
		  )

	 (ryo-modal-keys
	  ;; Leader key
	  ("SPC" (
				 ("c" (
						 ("s" save-buffer :name "Save Buffer")
						 ("o" write-file :name "Save As")
						 ("." dired :name "Dired")
						 ("a" magit-status :name "Git")
						 )
				  :name "File Motions"
				  )
				 ("t" (
						 ("t" scroll-up :name "Page Down")
						 ("T" zz-scroll-half-page-down :name "Half-Page Down")
						 ("c" scroll-down :name "Page Up")
						 ("C" zz-scroll-half-page-up :name "Half-Page Down")
						 ("u" recenter-top-bottom :name "Recenter Point")
						 ("e" move-to-window-line-top-bottom :name "Point at Center")
						 ;; ("e"
						 )
				  :name "Large Motions"
				  )								 ;		:name "Leader"
				 )
		:name "LEADER"
		)
	  )
	 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	'("7451f243a18b4b37cabfec57facc01bd1fe28b00e101e488c61e1eed913d9db9" default))
 '(line-number-mode nil)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp))))
