(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; list of must-to-have packages
(setq package-list '(evil
                     evil-leader
                     evil-visualstar
                     helm
                     highlight-numbers
                     magit
                     undo-tree))

(package-initialize)

;; check and install packages
(with-demoted-errors
  (when (or (file-exists-p package-user-dir) (package-refresh-contents))
    (dolist (package package-list)
      (unless (package-installed-p package)
        (package-install package)))))

;; evil mode
(require 'evil)
(evil-mode t)
; movements take into account visual line
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
; allow horizontal movement to cross lines
(setq-default evil-cross-lines t)
;use fine grain undo
(setq-default evil-want-fine-undo t)
; use default vim search 
(evil-select-search-module 'evil-search-module 'evil-search)
; space remove search highlight
(define-key evil-normal-state-map (kbd "SPC") 'evil-ex-nohighlight)
; allow to search visual selection
(global-evil-visualstar-mode)
; make region selection override search highlight:
; modify lisp/simple.el : redisplay-highlight-region-function
;   (overlay-put nrol 'priority '(10000 . 100)
; setup leader
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
; leader mappings:
; - save buffer
(evil-leader/set-key "," 'evil-write)
; - toggle underscore syntax meaning
(defun toggle-underscore-syntax ()
  (interactive)
  (if (char-equal (char-syntax ?_) ?_)
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?_ "_")))
(evil-leader/set-key "_" 'toggle-underscore-syntax)

;; bind vanilla emacs keystrokes
(global-set-key (kbd "M-x") 'helm-M-x)

;; ido-mode
(require 'ido)
(ido-mode t)

;; dired
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; ibuffer
(require 'ibuffer)
; - make default buffer listing
(defalias 'list-buffers 'ibuffer)
; - auto-refresh
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
; - size filed format
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
    ((> (buffer-size) 1000000000) (format "%7.1fG" (/ (buffer-size) 1000000000.0)))
    ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
    ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))

    (t (format "%8d" (buffer-size)))))
; - customize column size
(setq ibuffer-formats
  '((mark modified read-only " "
    (name 30 30 :left :nil) " "
    (size-h 9 -1 :right) " "
    (mode 16 16 :left :elide) " "
    filename-and-process)))

;; window options
; - move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; - move in window configuration history
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-c C-<left>") 'winner-undo)
  (global-set-key (kbd "C-c C-<right>") 'winner-redo))
; - resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; disable graphic features
;  - startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;  - menu bar
(menu-bar-mode -1)
;  - toolbar
(tool-bar-mode -1)
;  - scroll bars
(scroll-bar-mode -1)

;; auto revert mode
(global-auto-revert-mode 1)

;; confirm before closing
(setq confirm-kill-emacs 'y-or-n-p)

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;; scroll options
;  - mouse scroll 2 lines at a time
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
;  - don't accelerate mouse scrolling
(setq mouse-wheel-progressive-speed nil)
;  - scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;  - keyboard scroll one line at a time
(setq scroll-step 1)

;; color theme
(add-to-list 'custom-theme-load-path
  (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'oblivion t t)
(enable-theme 'oblivion)

;; do not set the background when opened in terminal
(defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
          (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; customize programming mode:
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; do not create backup/autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; TAB:
;  - do not insert '\t'
(setq-default indent-tabs-mode nil)
;  - tabulation is two spaces
(setq-default tab-width 2)
;  - tab key inserts tabulation  
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "<tab>") 'tab-to-tab-stop)))
;  - indentation is done with C-tab
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "<C-tab>") 'indent-for-tab-command)))

;; highlight matching parenthesis
(show-paren-mode 1)

;; Verilog customizations
; - disable automatic indentation
(defun ret-indent-relative ()
  (interactive)
  (newline)
  (indent-relative-maybe))
(eval-after-load 'verilog-mode
  '(progn
    (define-key verilog-mode-map (kbd ";") 'self-insert-command)
    (define-key verilog-mode-map (kbd "RET") 'ret-indent-relative)
    (define-key verilog-mode-map (kbd "TAB") 'tab-to-tab-stop)))
; - load veri-kompass
(when (file-exists-p "~/veri-kompass/")
    (add-to-list 'load-path "~/veri-kompass/")
  (require 'veri-kompass-mode))
; - load smime
(when (file-exists-p "~/midas/etc/SMIME/")
    (add-to-list 'load-path "~/midas/etc/SMIME/")
  (require 'smime))

;; save custom in a dedicated file and source it
(setq custom-file "~/.emacs.d/custom.el")
(write-region "" nil custom-file 'append)
(load custom-file)

;; TODO
;; - highlight TODOs
;; - set spellcheck
;; - missing remaps
