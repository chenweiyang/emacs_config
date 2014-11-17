;;some mode

;; gnu global
(autoload 'gtags-mode "gtags" "" t)

; c-mode common hook
(defun cwy/c-mode-common-hook()
  (subword-mode 1)
  (gtags-mode 1)
  ;(doxymacs-mode 1)
  (setq tab-width 8
	indent-tabs-mode t
	c-basic-offset 4)
  (local-set-key "\M-." 'gtags-find-tag)
  (local-set-key "\M-*" 'gtags-pop-stack)
  (local-set-key "\C-x4." 'gtags-find-tag-other-window))

(add-hook 'c-mode-common-hook 'cwy/c-mode-common-hook)

;; C++ mode common hook
(defun cwy/c++-mode-common-hook()
  (c-set-style "linux")
  (doxymacs-mode 1)
  (setq tab-width 4 
        indent-tabs-mode nil 
        c-basic-offset 4)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'friend '-))

;(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook 'cwy/c++-mode-common-hook)

;; java mode hook for ant
;(defun cwy/java-mode-common-hook()
;  (setenv "ANT_ARGS" "-emacs")
;  (setq compile-command "ant -find build.xml"))

;(add-hook 'java-mode-hook 'cwy/java-mode-common-hook)

;;Python-mode
;(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;(add-hook 'python-mode-hook
;           (lambda ()
;             (set (make-variable-buffer-local 'beginning-of-defun-function)
;                  'py-beginning-of-def-or-class)
;             (setq outline-regexp "def\\|class ")))

;(add-to-list 'load-path "~/hacking/lisp/slime")
;; boost slime load
;(setq slime-lisp-implementations
;           '((sbcl ("sbcl" "--core" "~/hacking/lisp/slime/sbcl.core-for-slime"))))
;(setq inferior-lisp-program "/usr/local/bin/sbcl"
;      lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slim-fuzzy-complete-symbol
;      common-lisp-hyperspec-root "file:///usr/share/common-lisp/HyperSpec/")
;(require 'slime-autoloads)
;(slime-setup '(slime-fancy slime-banner))

;;symbian c++ mode
;;(autoload 'symbian-c++-mode "symbian-c++-mode" "Symbian C++ mode" t)
;;(add-to-list 'auto-mode-alist '("\\.cpp\\'" . symbian-c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . symbian-c++-mode))


;; lua mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(defadvice lua-electric-match (around last-command-char-fixup activate) 
  (let ((last-command-char last-command-event)) 
    ad-do-it))
;; protobuf mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
;; indent 4
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
     (lambda () (c-add-style "my-pstyle" my-protobuf-style t)))


;; mecurial mode
(require 'mercurial)

;; go language mode
(require 'go-mode-load)

;; php mode
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php||'" . php-mode))
(defun my-php-mode-hook()
  (setq tab-width 4 
        indent-tabs-mode nil 
        c-basic-offset 4))
(add-hook 'php-mode-hook 'my-php-mode-hook)

;(defun load-ropemacs ()
;    "Load pymacs and ropemacs"
;    (interactive)
;    (require 'pymacs)
;    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
;    (setq ropemacs-confirm-saving 'nil)
;    )
;(global-set-key "\C-xpl" 'load-ropemacs)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

; yasnippet
(add-to-list 'load-path
             "~/emacs/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets" "~/emacs/yasnippet/snippets/"))
(yas-global-mode 1)


(require 'flymake)
;; Let's run 8 checks at once instead.
(setq flymake-max-parallel-syntax-checks 4)
;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
(require 'flymake-cursor)
;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)
(add-hook 'python-mode-hook 'flymake-mode-on)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) ()))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; clojure mode
(require 'clojure-mode)
;; nrep 
(require 'nrepl)
;; enable eldoc in clojure buffer
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
;; add auto-complete source for nrepl
;(require 'ac-nrepl)
;(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))

;(require 'ibus)
;(add-hook 'after-init-hook 'ibus-mode-on)


