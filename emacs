(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; list of must-to-have packages
(setq package-list '(evil
                     evil-leader
                     evil-visualstar
                     helm
                     highlight-numbers))

(package-initialize)

(or (file-exists-p package-user-dir) (package-refresh-contents))

;; install must-to-have packages if not present
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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
;(setq-default evil-want-fine-undo t)
; use default vim search 
(evil-select-search-module 'evil-search-module 'evil-search)
; space remove search highlight
(define-key evil-normal-state-map (kbd "SPC") 'evil-ex-nohighlight)
; allow to search visual selection
(global-evil-visualstar-mode)
; setup leader
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
; leader mappings:
; - save buffer
(evil-leader/set-key "," 'evil-write)

;; bind vanilla emacs keystrokes
(global-set-key (kbd "M-x") 'helm-M-x)

;; ido-mode
(require 'ido)
(ido-mode t)

;; window options
; - move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
; - move in window configuration history
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-c C-<left>") 'winner-undo)
  (global-set-key (kbd "C-c C-<right>") 'winner-redo))

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

; do not set the background when opened in terminal
(defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
          (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; customize look in programming mode:
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

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

;; do not wrap long lines
(global-visual-line-mode t)
;; highlight matching parenthesis
(show-paren-mode 1)

;; Verilog customizations
(when (file-exists-p "~/veri-kompass/")
    (add-to-list 'load-path "~/veri-kompass/")
  (require 'veri-kompass-mode))
(when (file-exists-p "~/midas-mode/")
    (add-to-list 'load-path "~/midas-mode/")
  (require 'midas-mode))

;; save custom in a dedicated file and source it
(setq custom-file "~/.emacs.d/custom.el")
(write-region "" nil custom-file 'append)
(load custom-file)

;; TODO
;; - highlight TODOs
;; - set spellcheck
;; - dump custom to separate custom.el 
;; - scrolling
;; - missing remaps
;; - modify plugin for visual select search-backwards 
;; - handle errors in package-install
;; - set nowrap
;; - add hook verilog: (define-key verilog-mode-map ";" 'self-insert-command)
