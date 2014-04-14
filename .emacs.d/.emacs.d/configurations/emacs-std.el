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

;; set default fill column 60
(setq default-fill-column 80)

;; set Tab
(setq-default indent-tabs-mode nil)
(setq defualt-tab-width 4)
(setq tab-stop-list())


;; set sentence-end with chinese character
(setq sentence-end "\\([¡££¡£¿]\\|¡­¡­\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;Dead !! with some file.DO NOT use it
;(setq scroll-margin 3 scroll-conservatively 10000)


;; set default mode to text-mode
(setq default-major-mode 'text-mode)

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
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)

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

;;set languages
;;(set-terminal-coding-system 'Chinese-gbk)
;;(set-keyboard-coding-system 'Chinese-gbk)
;;(set-language-environment 'Chinese-gbk)
;;(setq locale-coding-system 'Chinese-gbk)
;;(setq current-language-environment "Chinese-GBK")
;;(setq current-language-environment "Chinese-GB"
;;      locale-coding-system 'gb2312
;;      buffer-file-coding-system 'gb2312
;;      default-process-coding-system '(gb2312 . gb2312))

;;Set load coding
;(setq file-coding-system-alist '(
;;        ("\\.l31$" . "UTF-8")
;;))


;;set hippie list
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;Auto scroll *compilation* buffer to the end.
(setq compilation-scroll-output t)

;;Set color
(set-background-color "grey25")
(set-foreground-color "grey85")
(set-cursor-color "steelblue")
(set-mouse-color "slateblue")

;;Set for cygwin
;; This assumes that Cygwin is installed in C:\cygwin (the
;; default) and that C:\cygwin\bin is not already in your
;; Windows Path (it generally should not be).
;;
;;(setq exec-path (cons "f:/cygwin/bin" exec-path))
;;(setenv "PATH" (concat "f:\\cygwin\\bin;" (getenv "PATH")))
;;
;; NT-emacs assumes a Windows command shell, which you change
;; here.
;;
;;(setq shell-file-name "bash")
;;(setenv "SHELL" shell-file-name) 
;;(setq explicit-shell-file-name shell-file-name) 
;;
;; This removes unsightly ^M characters that would otherwise
;; appear in the output of java applications.
;;
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; Use the predefined fontset "fontset-standard"
(set-frame-font
 "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-110-*-*-m-0-iso10646-1")

;;(set-face-font 'default (create-fontset-from-fontset-spec
;; "-my-monaco-medium-r-normal--14-*-*-*-*-*-fontset-monaco,
;;    ascii:-*-Bitstream Vera Sans Mono-normal-r-*-*-13-*-*-*-*-*-iso8859-1,
;;    latin-iso8859-1:-*-Bitstream Vera Sans Mono-normal-r-*-*-13-*-*-*-*-*-iso8859-1,
;;    chinese-gb2312:-*-YouYuan-normal-r-*-*-13-*-*-*-*-*-gb2312*-*,
;;    chinese-big5-1:-*-MingLiU-normal-r-*-*-13-*-*-*-c-*-big5-*"))

;;(set-face-font 'default "-misc-dejavu sans mono-medium-r-normal--0-0-0-0-m-0-ascii-1")
;;(set-face-font 'default "-bitstream-bitstream vera sans mono-medium-r-*-*-*-100-*-*-*-*-*-*")
;(setq w32-enable-unicode-output nil)

;; hide toolbar
;;(tool-bar-mode -1)
;; hide scollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))



