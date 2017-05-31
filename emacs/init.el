;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Path to custom lisp packages
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load private variables that shouldn't be in a public config file
(load-library "secrets")

;; Set up package repos
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/") t)

;; We only need use-package when byte-compiling init.el
;; If you do not byte-compile, require use-package normally.
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Set up custom paths
(setenv "PATH" (concat "/opt/local/bin" ":"
                       (getenv "PATH")))
(add-to-list 'exec-path '"/opt/local/bin")
(add-to-list 'exec-path '"/opt/local/libexec/gnubin")
(add-to-list 'exec-path '"~/go/bin")

;; Don't display annoying default startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Tramp configuration
(use-package tramp
  :init
  ;; Probably not needed any more, uncomment if startup is very slow when
  ;; loading tramp files
  ;; (setq tramp-ssh-controlmaster-options "-o ControlPath=%%C -o ControlMaster=auto -o ControlPersist=no")

  :config
  (setq tramp-default-method "ssh")
  (defun tramp-set-auto-save () (auto-save-mode -1)) ;; do not auto-save over tramp
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave"))

;; Indentation defaults
(setq-default indent-tabs-mode nil) ;; no tabs
(setq tab-width 2) ;; 2 spaces per tab

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; Don't yell at me when creating a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Shut up!
(setq ring-bell-function 'ignore)

(use-package smerge-mode
  :config
  ;; Use C-c j as the smerge prefix because C-c ^ is hard to hit
  (setq smerge-command-prefix "\C-cj"))

;; Use CUA rectangle mode (C-RET), but without changing the copy/paste keys (see
;; http://irreal.org/blog/?p=1559)
(cua-selection-mode t)

;; Use the X clipboard for yanks
(setq
 select-enable-clipboard t
 select-enable-primary nil
 yank-excluded-properties t)

;; Disable GUI features. emacs in X is much nicer (fonts, etc.) but I don't
;; want menus taking up space.
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Mode-line customizations
(column-number-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)

;; Highlight matching parens
(show-paren-mode 1)

;; Place all backup files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Global key bindings
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-c c") 'compile)

;; Use C-j for tab complete
(global-set-key (kbd "C-j") 'dabbrev-expand)

;; Reuse compilation window
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*" . (display-buffer-reuse-window
                          . ((reusable-frames . t)))))

;; Enable winner-mode globally
(winner-mode 1)

;; Fix kill behavior
(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))
(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; Set up keymap for ipad ssh
;; Some ipad ssh apps send weird key code for various key combos. This mapping
;; makes things more reasonable.
;; (define-key key-translation-map [?\C-h] [?\C-?]) ; Unmask 'delete' as backspace
(let ((translations '( 229 [?\M-a]  8747 [?\M-b]   231 [?\M-c]  8706 [?\M-d]   nil [?\M-e]
                           402 [?\M-f]  169 [?\M-g]   729 [?\M-h]   nil [?\M-i]  8710 [?\M-j]
                           730 [?\M-k]  172 [?\M-l]   181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
                           960 [?\M-p]  339 [?\M-q]   174 [?\M-r]   223 [?\M-s]  8224 [?\M-t]
                           nil [?\M-u] 8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]   165 [?\M-y]
                           937 [?\M-z]
                           ; 96 [?\M-~] - didn't work, made backtick break
                           161 [?\M-1]   162 [?\M-4]   163 [?\M-3]   167 [?\M-6]
                           170 [?\M-9]  171 [?\M-\\]  175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
                           182 [?\M-7]  183 [?\M-\(]  186 [?\M-0]   187 [?\M-|]   191 [?\M-\?]
                           198 [?\M-\"] 230 [?\M-']   247 [?\M-/]   728 [?\M->]  8211 [?\M-\-]
                           8212 [?\M-_] 8216 [?\M-\]] 8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[]
                           8221 [?\M-{] 8225 [?\M-&]  8226 [\?M-8]  8249 [?\M-#]  8250 [?\M-$]
                           8260 [?\M-!] 8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]  8800 [?\M-=]
                           8804 [?\M-,] 8805 [?\M-.] 64257 [?\M-%] 64258 [?\M-^])))
  (while translations
    (let ((key (car translations)) (def (cadr translations)))
      (if key
          (define-key key-translation-map (make-string 1 key) def)))
    (setq translations (cddr translations))))


;; Scrolling
(set-scroll-bar-mode 'nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Line numbering
(global-linum-mode 1)
(setq column-number-mode t)
;; This is a hack to disable linum in some modes...
(setq linum-disabled-modes-list '(org-mode rcirc-mode term-mode shell-mode proced-mode compilation-mode Man-mode))
(defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

;; flyspell
(setq flyspell-issue-message-flag nil)

;; Save and restore session
(desktop-save-mode 1)
(setq desktop-restore-eager 5)

;; theming
(setq-default frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))

;; Colorize compilation buffer
(setq system-uses-terminfo nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :config
  ;; Load Dired X when Dired is loaded.
  (require 'dired-x)

  ;; Omit some files by default, but not while recovering a session
  (defun enable-dired-omit-mode () (dired-omit-mode 1))
  (add-hook 'dired-mode-hook 'enable-dired-omit-mode)
  (defadvice recover-session (around disable-dired-omit-for-recover activate)
    (let ((dired-mode-hook dired-mode-hook))
      (remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
      ad-do-it))

  ;; make dired reuse the same buffer
  (put 'dired-find-alternate-file 'disabled nil))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

;; Ivy completion replaces default emacs completion
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

;; Improves ivy for a few completions
(use-package counsel
  :ensure t)

;; Jump around a file based on regex's 
(use-package swiper
  :ensure t
  :bind ("M-s" . swiper))

(use-package paradox
  :ensure t
  :config
  (setq paradox-github-token t))

(use-package swiper
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "xb" 'ivy-switch-buffer)
  (key-chord-define-global "jm" 'set-mark-command))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init (key-chord-define-global "jj" 'ace-jump-mode))

;; win-switch seems to slow for my liking...
;; (use-package win-switch
;;   :ensure t
;;   :config (progn
;;             (win-switch-setup-keys-ijkl "\C-xo")
;;             (setq win-switch-other-window-first nil)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))
  (setq aw-scope 'frame)
  (key-chord-define-global "fj" 'ace-window))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-buffers "#.*")
  (projectile-mode 1))

;; I mostly use ivy/counsel for completion since it's faster and lighter weight,
;; but helm-M-x is nice and doesn't need to be that fast
(use-package helm
  :ensure t
  :config

  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; (global-set-key (kbd "C-x b") 'helm-mini)
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files             t
        helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t))

(use-package helm-ag
  :ensure t
  :bind ("C-x a" . helm-do-ag-project-root))

(use-package disaster
  :ensure t
  :commands (disaster)
  :init
  (progn
    (defun my-c-initialization-hook ()
      (define-key c-mode-base-map (kbd "C-c d") 'disaster))
    (add-hook 'c-initialization-hook 'my-c-initialization-hook)))

(use-package dsvn
  :ensure t
  :commands (svn-status svn-update))

(use-package ecb
  :ensure t
  :commands (ecb-activate ecb-minor-mode))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode))

(use-package rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-use-helm t)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  (rtags-enable-standard-keybindings))

(use-package company
  :ensure t
  :config
  (add-to-list
   'company-backends 'company-rtags))

(use-package helm-rtags
  :ensure t)

(use-package company-rtags
  :ensure t)

(use-package find-file-in-project
  :ensure t
  :commands (find-file-in-project)
  :init
  (defalias 'ffip 'find-file-in-project))

(use-package sr-speedbar
  :ensure t
  :commands (sr-speedbar-open sr-speedbar-toggle))

(use-package which-func
  :ensure t
  :config (setq which-func-modes '(c-mode c++-mode org-mode java-mode)))

(use-package magit
  :bind ("C-c m" . magit-status)
  :ensure t
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    ;; (setq magit-completing-read-function
    ;;       'magit-ido-completing-read)
    (setq auto-revert-remote-files nil))
    ;; (mapc (apply-partially 'add-to-list 'magit-repo-dirs)
    ;;       '("git" "repos" "here"))
  )


;; Writegood
(use-package writegood-mode
  :commands (writegood-mode))

(use-package which-func
  :config
  (which-function-mode 1)
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info)))

(use-package synonyms
  :ensure t
  :init (setq
         synonyms-file "~/.emacs.d/mthesaur.txt"
         synonyms-cache-file "~/.emacs.d/mthesaur.txt.cache"))

(use-package openwith
  :config
  (progn
    (setq openwith-associations '(("\\.pdf\\'" "okular" (file))))
    (openwith-mode t)))

(use-package windmove
  :config
  (windmove-default-keybindings))


;; OS specific stuffs
(with-eval-after-load 'mac-print-mode
  (use-package mac-print-mode
    :config
    (progn
      (global-set-key (kbd "M-p") 'mac-print-buffer)
      (mac-print-mode 1))))

(when (eq system-type 'darwin) ;; mac specific settings
  (setq python-python-command "/opt/local/bin/python2")
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(setq csv-separators '("," "	" ";"))

;; Doc viewing
(setq doc-view-resolution 72)
(setq doc-view-scale-internally t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(font . "InconsolataDZ-12"))
      (set-face-font 'default "InconsolataDZ-12"))
  (progn
    (set-face-font 'default "Inconsolata-11")))

(use-package man
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package rich-minority
  :ensure t
  :config
  (setq rm-blacklist '(" hl-p" " Helm" " HelmGtags" " GG" " Abbrev" " Projectile" " ivy" " ARev")))

;; Code snippet insertion
(use-package yasnippet
  :ensure t
  :config
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate t)))
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;
;; Source code modes ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package gyp
  :mode (
         ("\\.gyp\\'" . gyp-mode)
         ("\\.gypi\\'" . gyp-mode)))

(use-package cmake-mode
  :ensure t
  :mode (
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package markdown-mode
  :ensure t
  :mode (
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-flyspell)
    (add-hook 'markdown-mode-hook 'visual-line-mode)))

(use-package rw-hunspell
  :ensure t
  :config
  (progn
    (setenv "LANG" "en_US")
    (setq ispell-program-name "hunspell"
          rw-hunspell-default-dictionary "en_US"
          rw-hunspell-make-dictionary-menu t
          rw-hunspell-use-rw-ispell t
          ispell-dictionary "en_US")
    (rw-hunspell-setup)))

(use-package google-c-style
  :ensure t
  :config
  (c-add-style "Google" google-c-style))

;; LLVM modes
(use-package llvm-mode)
(use-package tablegen-mode)

;; LLVM coding style guidelines in emacs
;; Maintainer: LLVM Team, http://llvm.org/

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
	  (function
	   (lambda nil 
	     (if (and buffer-file-name (string-match "llvm" buffer-file-name))
		 (progn
		   (c-set-style "llvm.org"))))))

;; Google C style
;; from https://raw.githubusercontent.com/google/styleguide/gh-pages/google-c-style.el
(use-package google-c-style)

;; Latex
(defun okular-make-url () (concat
		"file://"
		(expand-file-name (funcall file (TeX-output-extension) t)
			(file-name-directory (TeX-master-file)))
		"#src:"
		(TeX-current-line)
		(TeX-current-file-name-master-relative)))

(defun skim-make-url ()
  (concat
   (TeX-current-line)
   " \""
   (expand-file-name (funcall file (TeX-output-extension) t)
             (file-name-directory (TeX-master-file)))
   "\" \""
   (buffer-file-name)
   "\""))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex$" . latex-mode)
  :config
  (progn
    (setq TeX-PDF-mode t)
    (setq LaTeX-command "latex -synctex=1")
    (setq reftex-plug-into-AUCTeX t)
    (setenv "PATH" (concat "/usr/texbin" ":" "/Library/TeX/texbin" ":"
                           (getenv "PATH")))
    (setq TeX-view-program-list
          '(("Evince" "evince %u")
            ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
    (if (eq system-type 'gnu/linux)
        (progn
          (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))))
    (if (eq system-type 'darwin)
        (progn
          (setq TeX-view-program-selection '((output-pdf "Skim")))))
    (setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting"))
    (dolist (mode '(visual-line-mode
                    turn-on-flyspell
                    LaTeX-math-mode
                    writegood-mode
                    zotelo-minor-mode
                    turn-on-reftex
                    TeX-fold-mode
                    (lambda ()
                      (add-hook 'find-file-hook 'TeX-fold-buffer t))))
      (add-hook 'LaTeX-mode-hook mode))))


;; Java/CoffeeScript
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config (setq js-indent-level 2))

(use-package js-mode
  :mode ("\\.ljs$" . js-mode)
  :config (setq js-indent-level 2))

(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode)
  :config (define-key go-mode-map (kbd "M-.") 'godef-jump))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode (("\\.hs$" . haskell-mode))
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (setq haskell-process-type 'cabal-repl)
    (setq haskell-program-name "ghci \"+.\"")))

;; Scheme
(use-package scheme-mode
  :mode "\\.lm\\'"
  :config (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

;; C/C++
(setq c-default-style "k&r"
      c-basic-offset 2
      c++-indent-level 2)

(setq whitespace-global-modes '(c-mode c++-mode))
(setq whitespace-style '(face trailing lines-tail empty))

(add-hook 'java-mode-hook
	  (lambda()
	    (set (make-local-variable 'compile-command) (concat "javac " (buffer-name)))))

;; Emacs Prolog
;; http://bruda.ca/emacs/prolog_mode_for_emacs
(use-package prolog
  :config
  :mode (("\\.pl$" . prolog-mode)
         ("\\.m$" . mercury-mode)))

;; HTML/CSS
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.bp$" . json-mode)))


;; Terminal configuration

(use-package ansi-color
  :ensure t
  :config
  (progn
    (setq ansi-color-for-comint-mode t)
    (setq compilation-scroll-output 'first-error)
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

(use-package multi-term
  :bind (("C-c t" . multi-term)
         ("C-<f12>" . multi-term-dedicated-toggle)
         ("C-c n" . multi-term-next)
         ("C-c p" . multi-term-prev))

  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-dedicated-select-after-open-p t))

;; Don't allow modification of the eterm prompt
(setq comint-prompt-read-only t)

;; Various useful shortcuts bound to maps

(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "a" #'adaptive-wrap-prefix-mode)
;; (define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "l" #'linum-mode)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "t" #'endless/toggle-theme)
;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(define-prefix-command 'launcher-map)
;; C-x l is `count-lines-page' by default. If you
;; use that, you can try s-l or <C-return>.
(define-key ctl-x-map "l" 'launcher-map)
;; (global-set-key (kbd "s-l") 'launcher-map)
(define-key launcher-map "c" #'calc)
(define-key launcher-map "d" #'ediff-buffers)
(define-key launcher-map "f" #'find-dired)
;; (define-key launcher-map "G" #'lgrep)
(define-key launcher-map "g" #'rgrep)
(define-key launcher-map "h" #'man) ; Help
(define-key launcher-map "i" #'package-install-from-buffer)
(define-key launcher-map "n" #'nethack)
(define-key launcher-map "p" #'paradox-list-packages)
(define-key launcher-map "s" #'shell)
(define-key launcher-map "t" #'proced) ; top

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))


;; ICIRC stuff
;; Customize rcirc-server-alist to set up IRC servers and accounts.
(use-package rcirc
  :defer 5
  :config
  (setq rcirc-buffer-maximum-lines 4096)

  (rcirc-track-minor-mode 1)
  (rcirc nil))


;; Org-mode stuff

(use-package org-plus-contrib)

; Persistent clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; BEGIN org-mode efficiency customizations

;; This library is a large set of customizations to org-mode from
;; http://doc.norang.ca/org-mode.html
(load-library "org-mode")
(global-set-key (kbd "C-c v") 'org-capture)
(global-set-key (kbd "C-c C-v") 'org-capture)

(setq org-startup-indented t)

;; Customize templates
(setq org-structure-template-alist
      (quote (("s" "#+BEGIN_SRC ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>")
              ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE" "<example>\n?\n</example>")
              ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOT" "<quote>\n?\n</quote>")
              ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
              ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER" "<center>\n?\n</center>")
              ("l" "#+BEGIN_LATEX\n?\n#+END_LATEX" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+LATEX: " "<literal style=\"latex\">?</literal>")
              ("h" "#+BEGIN_HTML\n?\n#+END_HTML" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
              ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII")
              ("A" "#+ASCII: ")
              ("i" "#+INDEX: ?" "#+index: ?")
              ("I" "#+INCLUDE %file ?" "<include file=%file markup=\"?\">"))))

;; Add shstream to org-babel
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t)
         (shstream . t))))

;; Print current org clock info to a file. I display this file in my window
;; manager status line.
(defun esf/org-clocking-info-to-file ()
  (with-temp-file "~/tmp/clocking"
    ;; (message (org-clock-get-clock-string))
    (if (org-clock-is-active)
        ;; (insert (format "org: %d/%d min" 
        ;;                 (- (org-clock-get-clocked-time) org-clock-total-time)
        ;;                 (org-clock-get-clocked-time))
        ;;         )
        (insert (org-clock-get-clock-string))
      ) ;;(org-clock-get-clock-string)
    )
  )
(when (not (eq system-type 'darwin))
  (add-hook 'display-time-hook 'esf/org-clocking-info-to-file))

;; END org-mode efficiency customizations


(setq org-log-done t)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)


;; fontify code in code blocks
(setq org-src-fontify-natively t)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         (reftex-parse-all)
         ;add a custom reftex cite format to insert links
         (reftex-set-cite-format "** [[papers:%l][%l]]: %t \n"
                                 )))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
(add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode)
(add-hook 'docview-mode 'turn-on-auto-revert-mode)
(add-hook 'org-mode-hook 'org-mode-reftex-setup)
 
(use-package org-trello
  :ensure t
  :config
  (setq org-trello-current-prefix-keybinding "C-c o"))



;; Make large files read-only by default to speed up loading
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    ; (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)


;; Package I used to use, but have since replaced. Keeping these customizations
;; here in case anyone prefers these packages.

;; IDO
;; (use-package ido
;;   :config
;;   (progn
;;     (ido-mode t)
;;     (ido-everywhere t)
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-create-new-buffer 'always)
;;     (setq ido-enable-tramp-completion nil)
;;     (setq ido-confirm-unique-completion nil)
;;     (setq ido-default-buffer-method 'selected-window)))

;; (use-package xcscope
;;   :ensure t
;;   :commands (cscope-setup)
;;   :init (setq cscope-program "gtags-cscope")
;;   :config (cscope-setup))

;; (use-package ggtags
;;   :ensure t
;;   :commands (ggtags-mode)
;;   :init
;;   (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))
;;   (setq ggtags-update-on-save nil)
;;   (add-hook 'after-save-hook 'gtags-update-hook))

;; (use-package helm-gtags
;;   :commands (helm-gtags-mode)
;;   :init
;;   (setq
;;    helm-gtags-ignore-case t
;;    helm-gtags-auto-update t
;;    helm-gtags-use-input-at-cursor t
;;    helm-gtags-pulse-at-cursor t
;;    helm-gtags-prefix-key "\C-cg"
;;    helm-gtags-suggested-key-mapping t
;;    )
;;   (add-hook 'dired-mode-hook 'helm-gtags-mode)
;;   (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c-mode-hook 'helm-gtags-mode)
;;   (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;   (add-hook 'asm-mode-hook 'helm-gtags-mode)

;;   :config
;;   ;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;;   ;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;;   (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;;   (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;   (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;   (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;   )

;; (defun gtags-root-dir ()
;;   "Returns GTAGS root directory or nil if doesn't exist."
;;   (with-temp-buffer
;;     (if (zerop (call-process "global" nil t nil "-pr"))
;;         (buffer-substring (point-min) (1- (point-max)))
;;       nil)))

;; (defun gtags-update-single(filename)
;;   "Update Gtags database for changes in a single file"
;;   (interactive)
;;   (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

;; (defun gtags-update-current-file()
;;   (interactive)
;;   (defvar filename)
;;   (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
;;   (gtags-update-single filename)
;;   (message "Gtags updated for %s" filename))

;; (defun gtags-update-hook()
;;   "Update GTAGS file incrementally upon saving a file"
;;   (when helm-gtags-mode
;;     (when (gtags-root-dir)
;;       (gtags-update-current-file))))


;; (use-package cc-mode)

;; (use-package speedbar
;;   :config
;;   (setq speedbar-tag-hierarchy-method '(speedbar-sort-tag-hierarchy)))
;; (use-package semantic
;;   :config
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))
;;   (semantic-mode 1))

;; (use-package stickyfunc-enhance
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Theming
(use-package color-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn)
  
  (defun sjc/zenburn-color (name)
    (cdr (assoc name zenburn-colors-alist)))

  (setq org-todo-keyword-faces
        `(("TODO" :foreground ,(sjc/zenburn-color "zenburn-red") :weight bold)
          ("NEXT" :foreground ,(sjc/zenburn-color "zenburn-yellow") :weight bold)
          ("DONE" :foreground ,(sjc/zenburn-color "zenburn-green") :weight bold)
          ("WAITING" :foreground ,(sjc/zenburn-color "zenburn-orange") :weight bold)
          ("HOLD" :foreground ,(sjc/zenburn-color "zenburn-magenta") :weight bold)
          ("CANCELLED" :foreground ,(sjc/zenburn-color "zenburn-blue") :weight bold)
          ("MEETING" :foreground ,(sjc/zenburn-color "zenburn-blue") :weight bold)
          ("PHONE" :foreground ,(sjc/zenburn-color "zenburn-blue") :weight bold))))

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init (sml/setup)
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:") t)
  (add-to-list 'sml/replacer-regexp-list '("\\\@.*bitlbee$" "") t)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 32)
  (setq sml/mode-width 'full)
  (sml/apply-theme 'dark))

;; Start a server so we can connect with emacsclient as needed
(server-start)
