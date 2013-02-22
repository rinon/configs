; ELPA setup
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; Location of custom plugins       
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp") load-path))

; Tabbing
(setq-default indent-tabs-mode nil)
(setq c-default-style "k&r"
      c-basic-offset 2)
(setq default-tab-width 2)
  ; for JavaScript
(setq js-indent-level 2)

; Font
(if (eq system-type 'darwin)
    (set-default-font "Inconsolata-dz-12")
  (set-default-font "Inconsolata-dz-10"))

; Use emacs style copy+paste
(cua-selection-mode t)

;(menu-bar-mode 0)
(tool-bar-mode 0)

; Allows dired to reuse buffer when opening sub-folder using 'a'
(put 'dired-find-alternate-file 'disabled nil)

; Save my buffers
(desktop-save-mode 1)

; org-mode stuff
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-directory "~/org")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files (quote ("dinner.org")))
(setq org-log-done t)
; Persistent clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


; Display things that I want
(column-number-mode t)
(display-battery-mode t)
(setq display-time-24hr-format t)
(display-time-mode t)
(setq inhibit-splash-screen t)
(set-scroll-bar-mode 'nil)
(show-paren-mode 1)

(global-linum-mode 1)
(setq column-number-mode t)
(setq linum-disabled-modes-list '(org-mode)) (defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))


; Occasionally want to insert the date
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%m/%d/%Y")
		 ((equal prefix '(4)) "%c"))))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

; Compilation binds and options
(add-hook 'java-mode-hook
	  (lambda()
	    (set (make-local-variable 'compile-command) (concat "javac " (buffer-name)))))

(global-set-key (kbd "C-c c") 'compile)
(setq compilation-scroll-output 'first-error)

; Haskell modes
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; Paren matching
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		(t (self-insert-command (or arg 1)))))

; Write-good mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

; LaTeX stuff
(setq TeX-PDF-mode t)
(setq LaTeX-command "latex -synctex=1")

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'speck-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'writegood-mode)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setenv "PATH" (concat "/usr/texbin" ":"
                       (getenv "PATH")))

(setenv "PATH" (concat "/opt/local/bin" ":"
                       (getenv "PATH")))
(setq exec-path (append exec-path '("/opt/local/bin")))

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

(setq TeX-view-program-list '(("Evince" "evince %u") ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

(if (system-type-is-gnu)
    (setq TeX-view-program-selection '((output-pdf "Evince") (output-dvi "Evince"))))

(if (system-type-is-darwin)
    (setq TeX-view-program-selection '((output-pdf "Skim"))))


(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list '("%u" okular-make-url))
     (add-to-list 'TeX-expand-list '("%q" skim-make-url))))

; --------- End of LaTeX configs


; Web dev modes
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ljs$" . js-mode))

(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(add-to-list 'auto-mode-alist '("\\.ctp\\'" . html-mode)) ; CakePHP template file



; Markdown modes
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook 'speck-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)

; IDO configuration
(setq confirm-nonexistent-file-or-buffer nil)
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-confirm-unique-completion nil)


; Misc
(setq tramp-default-method "ssh")
(setq make-backup-files nil)
(setq fill-column 80)

(if (system-type-is-darwin)
    (setq python-python-command "/opt/local/bin/python2"))


;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

; Sets ctrl-i to tab complete
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'dabbrev-expand)

(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))



; theme stuff
(setq-default frame-background-mode 'dark)

; Need to init packages before we can set the theme
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((py-which-shell . "python2")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; for multiple sessions
(server-start)
