;;some mode

;; C++ code style
(defun my-c++-mode-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "stroustrup")
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'friend '-)
  )
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

;;imenu
;;(add-hook 'c-mode-hook 'imenu-add-menubar-index)
;;(add-hook 'c++-mode-hood 'imenu-add-menubar-index)
;;(add-hook 'java-mode-hood 'imenu-add-menubar-index)

;;todo mode
(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode"
  "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)

;;Python-mode
;(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\`Sc" . python-mode))
(add-to-list 'auto-mode-alist '("\\`SC" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;(add-hook 'python-mode-hook
;           (lambda ()
;             (set (make-variable-buffer-local 'beginning-of-defun-function)
;                  'py-beginning-of-def-or-class)
;             (setq outline-regexp "def\\|class ")))

;;nXml-mode
;(load "rng-auto")
;(add-to-list 'auto-mode-alist
;             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
;                   'nxml-mode))

;;desktop-save mode
;;(desktop-save-mode nil)
;; when locked, just load
;;(add-to-list 'desktop-load-locked-desktop t)
;;(setq history-length 250)
;;(add-to-list 'desktop-globals-to-save 'file-name-history)
;;(desktop-read)
;;(setq desktop-buffers-not-to-save
;;      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
;;              "\\)$"))
;;(add-to-list 'desktop-modes-not-to-save 'dired-mode)
;;(add-to-list 'desktop-modes-not-to-save 'Info-mode)
;;(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;;(add-to-list 'desktop-modes-not-to-save 'fundamental-mo)

;(add-to-list 'load-path "~/src/slime/")
;(require 'slime)
;(add-hook 'lisp-mode-hook (lambda() (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))
;(setq inferior-lisp-program "/usr/bin/sbcl"
;      lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slim-fuzzy-complete-symbol
;      common-lisp-hyperspec-root "file:///usr/share/common-lisp/HyperSpec/")

;;symbian c++ mode
;;(autoload 'symbian-c++-mode "symbian-c++-mode" "Symbian C++ mode" t)
;;(add-to-list 'auto-mode-alist '("\\.cpp\\'" . symbian-c++-mode))
;;(add-to-list 'auto-mode-alist '("\\.h\\'" . symbian-c++-mode))

;; slime
(setq inferior-lisp-program "C:/Program Files/Steel Bank Common Lisp/1.0.9/sbcl.exe")
(add-to-list 'load-path "~/.emacs.d/extensions/slime")
(require 'slime)
(slime-setup)

;; lua mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

;; protobuf mode
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
;; indent 4
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
     (lambda () (c-add-style "my-pstyle" my-protobuf-style t)))


;; cedet
(add-to-list 'load-path "~/.emacs.d/extensions/cedet_1.0/common")
(require 'cedet)
(require 'semantic-ia)

(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(setq semanticdb-default-save-directory "~/.emacs.d/")
(global-ede-mode 1)
(which-func-mode 0)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../fromyy" "common" "fromyy"
        "../.." "../../include" "../../inc" "../../common" ))
(defconst cedet-win32-include-dirs
  (list "C:/MinGW/include"
        "C:/MinGW/include/c++/3.4.5"
        "C:/MinGW/include/c++/3.4.5/mingw32"
        "C:/MinGW/include/c++/3.4.5/backward"
        "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
(require 'semantic-c nil 'noerror)
(let ((include-dirs cedet-user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

(setq semanticdb-search-system-databases t)
;;(semantic-idle-completions-mode nil)
;(setq semantic-complete-inline-analyzer-idle-displayor-class 
;      'semantic-displayor-traditional)


(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-o" 'eassist-list-methods)
  (local-set-key [(control tab)] 'eassist-switch-h-cpp)
  (local-set-key [f3] 'semantic-decoration-include-visit))

(add-hook 'c++-mode-hook 'my-cedet-hook)
