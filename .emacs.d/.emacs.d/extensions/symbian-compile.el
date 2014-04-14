;;Symbian program need use abld.bat file in the "group"
;;folder under project folder.

(defun symbian-compile-sdk ()
  "Build with RD sdk"
  (interactive)
  (symbian-build "/epoc32/tools;/epoc32/gcc/bin;"
                 "/build_symbian"
                 "../"
                 "abld build winscw udeb"))

(defun symbian-bldmake-bldfiles ()
  "bldmake bldfiles"
  (interactive)
  (symbian-build ""
                 "/build_symbian"
                 "../"
                 "bldmake bldfiles WINSCW ARMV5"))

(defun symbian-build (addpath abldpath prjroot cmpl-cmd)
  "Base build"
  (let ((old-path-env (getenv "PATH"))
        (old-exec-path exec-path))
    (cd (concat (expand-file-name prjroot) abldpath))
    (if (not (file-exists-p "abld.bat"))
        (message "Need a abld.bat file!!"))
    ;(setenv "PATH" (concat addpath (getenv "PATH")))
    (setenv "EPOCROOT" "\\S60\\devices\\S60_5th_Edition_SDK_v1.0\\")
    (setq exec-path (cons "/epoc32/tools" exec-path))
    (set 'compile-command cmpl-cmd)
    ;(call-interactively 'set-shell-cmdproxy)
    (call-interactively 'compile)
    ;(call-interactively 'set-shell-bash)
    (setq exec-path  old-exec-path)))

(define-skeleton cwy-skeleton-symbian-new-ctype-class
  "generate a new C-type class" "Class Name:"
  > "/**" str "class declare" "\n*\n*\n*/"
  > "\nclass " str " : public " 
  > "\n{"
  > "\npublic: //Constructor and Destructor" 
  > "\n" str "();"
  > "\n"
  > "\nvirtual ~" str "();"
  > "\n"
  > "\n//tow-phrase constructor"
  > "\nvoid ConstructL();"
  > "\n"
  > "private: //Implementation"
  > "\n"
  > "private: //Data"
  > "\n};"
  > "\n"
  )

(define-skeleton cwy-skeleton-symbian-insert-fuction-comment
  "Insert a function implementation comment" nil
  > "//---------------------------------------------------------------------"
  > "\n//"
  > "\n//---------------------------------------------------------------------"
  > "\n//"
  )


(provide 'symbian-compile)



