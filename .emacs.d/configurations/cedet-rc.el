;; cedet
(setq cedet-root-path "~/src/cedet/")
(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

;; Enable Semantic
(semantic-mode 1)
(require 'semantic/bovine/gcc)
(semantic-add-system-include "/usr/incldue/asio" 'c++-mode)
;; load contrib library
(require 'eassist)


(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
;; use gnu global 
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; key bind
;; customisation of modes
(defun cwy/cedet-hook ()
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

(add-hook 'c-mode-common-hook 'cwy/cedet-hook)

(defun cwy/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'cwy/c-mode-cedet-hook)

(defun recur-list-files (dir re)
  "Returns list of files in directory matching to given regex"
  (when (file-accessible-directory-p dir)
    (let ((files (directory-files dir t))
          matched)
      (dolist (file files matched)
        (let ((fname (file-name-nondirectory file)))
          (cond
           ((or (string= fname ".")
                (string= fname "..")) nil)
           ((and (file-regular-p file)
                 (string-match re fname))
            (setq matched (cons file matched)))
           ((file-directory-p file)
            (let ((tfiles (recur-list-files file re)))
              (when tfiles (setq matched (append matched tfiles)))))))))))

(defun c++-setup-boost (boost-root)
  (when (file-accessible-directory-p boost-root)
    (let ((cfiles (recur-list-files boost-root "\\(config\\|user\\)\\.hpp")))
      (dolist (file cfiles)
        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))

;(require 'semantic-lex-spp)
(global-ede-mode t)
(ede-enable-generic-projects)
;; EDE project
(when (file-exists-p "~/work/proxy-server/.hgignore")
  (setq cpp-proxy-server-project
        (ede-cpp-root-project "ProxyServer"
                              :name "YY Proxy Server"
                              :file "~/work/proxy-server/.hgignore"
                              :include-path '("/"
                                              "/proxyserver"
                                              "/common"
                                              "/mobiletask"
                                              "/liveclient"
                                              "/webclient"
                                              "/asyncserver"
                                              )
                              :local-variables (list
						(cons 'compile-command "scons -D")
						))))


;; Setup JAVA....
(require 'cedet-java)
(require 'semantic/db-javap)
;; malabar
;(add-to-list 'load-path "~/.emacs.d/extensions/malabar/lisp")
;(require 'malabar-mode)
;(setq malabar-groovy-lib-dir "~/.emacs.d/extensions/malabar/lib")
;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))


;; ecb
;(add-to-list 'load-path "~/src/ecb")
;(require 'ecb)

;; fix compilation-mode for maven2
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'maven2)
(add-to-list 'compilation-error-regexp-alist-alist
             '(maven2 "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
               1 2 3))


;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/extensions/ac-dict")
;; Add ac-source-dictionary to ac-sources of all buffer
(defun cwy/ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags) ac-sources)))

(defun cwy/ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-variables ac-source-symbols) ac-sources)))

(defun cwy/ac-java-mode-setup ()
  (setq ac-sources '(ac-source-gtags ac-source-words-in-same-mode-buffers)))

(defun cwy/ac-config-default ()
  (setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'cwy/ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'cwy/ac-cc-mode-setup)
  (add-hook 'java-mode-hook 'cwy/ac-java-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(cwy/ac-config-default)
