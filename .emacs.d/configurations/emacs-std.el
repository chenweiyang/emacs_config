;;The stand emacs config file
;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; disable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; default to unified diffs
(setq diff-switches "-u")

;; disable bell
(setq visible-bell t)

;; close startup message
(setq inhibit-startup-message t)

;; show column number
(setq column-number-mode t)

(setq mouse-yank-at-point t)

;; big kill-ring
(setq kill-ring-max 200)

;; set default fill column 80
(setq fill-column 80)
(setq-default fill-column 80)

;; set Tab
(setq defualt-tab-width 4)
(setq tab-stop-list())

;; set parentheses
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; move auto mouse when input
(mouse-avoidance-mode 'animate)

;; set title to buffer name
(setq frame-title-format "emacs@%b")

;; set auto open image file
(auto-image-file-mode)

;; open disable function
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-enviroment 'disabled nil)

;; set backup version
(setq version-control t)
(setq kept-new-versions 6)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)
(setq backup-directory-alist '(("." . "~/.saves")))

;; set dired to recurisive
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; resize minibuffer
;(resize-minibuffer-mode)
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "chinese-py-punct")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t nil (paren)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;;set hippie list
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;Auto scroll *compilation* buffer to the end.
(setq compilation-scroll-output t)

;;Set color
(set-background-color "grey25")
(set-foreground-color "grey85")
(set-cursor-color "steelblue")
(set-mouse-color "slateblue")

(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; Use the predefined fontset "fontset-standard"
;(set-frame-font
; "-apple-inconsolata-medium-r-normal--17-130-72-72-m-130-iso10646-1")


;; hide toolbar
(tool-bar-mode -1)
;; hide scollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable auto-save for god sake of ssd :(
(setq auto-save-default nil)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; The OS X Terminal.app uses UTF-8 by default.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; browser-kill
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;Set C-z to unset
(global-unset-key [?\C-z])

;;Set debug key for gdb
(add-hook 'gdb-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "<f10>") 'gud-next)
             (define-key c-mode-base-map (kbd "<f11>") 'gud-step)
             (define-key c-mode-base-map (kbd "<f5>")  'gud-go)
             (define-key c-mode-base-map (kbd "<f6>")  'gud-cont)
             (define-key c-mode-base-map (kbd "<f9>")  'gud-break)
             (define-key c-mode-base-map (kbd "S-<f9>") 'gud-remove)))  

;(require 'doxymacs)

