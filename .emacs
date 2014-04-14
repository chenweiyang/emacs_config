;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

(setq load-path 
      (cons "~/.emacs.d/configurations" 
            (cons "~/.emacs.d/extensions" load-path)))
(add-to-list 'load-path "~/src/cc-mode/")
(add-to-list 'load-path "~/src/doxymacs-1.8.0/lisp/")
(load "emacs-std") 
(load "cedet-rc")
(load "modes") 
(load "extensions")
;(load "emacs-for-python")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(cc-other-file-alist (quote (("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.c\\'" (".h")) ("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".h" ".hh" ".hhp")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(diff-switches "-ubra")
 '(ede-maven2-maven-options (quote ("-B" "-o")) nil (ede/maven2))
 '(ediff-diff-options "--binary --ignore-space-change")
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(glasses-face (quote bold))
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-font-lock-mode t nil (font-lock))
 '(safe-local-variable-values (quote ((Package . XREF) (Syntax . Common-lisp))))
 '(semantic-idle-scheduler-idle-time 3)
 '(show-paren-mode t nil (paren))
 '(user-mail-address "chenweiyang@yy.com"))


(defun screen-width nil -1)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:foreground "Yellow" :underline t)))))
