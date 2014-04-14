;;extension file

;; use iswitchlib to name a buffer
(require 'iswitchb)
(iswitchb-mode t)
;;(iswitchb-default-keybindings)

;; use desktop lib
;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)

;; Load CEDET
;;(load-file "~/.emacs.d/extensions/cedet-1.0pre4/common/cedet.el")
;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following
;(semantic-load-enable-code-helpers)
;(semantic-load-enable-guady-code-helpers)
;(semantic-load-enable-excessive-code-helpers)

;; IMPORTANT! to prevent cedet hang emcas when open a file in Python Mode
(eval-after-load "wisent-python"
  (remove-hook 'python-mode-hook 'wisent-python-default-setup))

;; Load Emacs Code Browser which NEED cedet be loaded first
;; (require 'ecb-autoloads)

;;ascii table
(require 'ascii)

;;chinese calender
(require 'chinese-calendar)

;;backup-dir
(require 'backup-dir)
(setq bkup-backup-directory-info
      '((t "~/backups" ok-create full-path)))
   
;;auto font menu
(autoload 'alt-mouse-set-font "alt-font-menu"
       "interactively choose font using mouse" t)
;;Set shift down-mouse-1 
(global-set-key [(shift down-mouse-1)] 'alt-mouse-set-font)

;;Load symbian-compile
(require 'symbian-compile)

;;load sourcepair to find a header file or souce file
(require 'sourcepair)
(setq sourcepair-source-path    '( "../src/*" "."))
(setq sourcepair-header-path    '( "../inc/*" "." "include" "../include" "../../epoc32/include/*"))
(setq sourcepair-recurse-ignore '( "CVS" "Obj" "Debug" "Release" "SVN"))
(global-set-key "\C-x\C-c" 'sourcepair-load)

;; TRAMP
(require 'tramp)
(setq tramp-default-method "scp")
(setq tramp-default-user "cwy")