;; Symbian C++ major mode for Emacs.
;;
;; Each Emacs buffer has exactly one major mode active at all times.
;; You may want to use this mode when editing source code that
;; has been formatted according to the Symbian coding standards.
;;
;; Copyright 2003-2004 by Tero Hasu.
;;
;; symbian-c++-mode.el is free software distributed under
;; the terms of the GNU General Public Licence, version 2
;; (not any later version). For details see the file COPYING.

;; USE:
;;
;; Mark those files that are Symbian-style C++ by prepending
;; the following line:
;; // -*- symbian-c++ -*-
;; (A utility function for doing so is provided.)

;; INSTALLATION:
;;
;; To install, ensure that the symbian-c++-mode.el (or elc)
;; file is somewhere on the load path, and have the following code 
;; evaluated during Emacs startup:
;; (autoload 'symbian-c++-mode "symbian-c++-mode" "Symbian C++ mode" t)

;; DEVELOPMENT GUIDELINES:
;;
;; All names declared in this file must begin with 'symbian-c++-'.

;; TODO:
;;
;; * coloring of Symbian-specific keywords and maybe some of
;;   the more common types
;; * a mode-specific menu (perhaps)

(define-derived-mode symbian-c++-mode 
  c++-mode "Symbian C++"
  "Major mode for Symbian-style C++ code.
  \\{symbian-c++-mode-map}"

  ;; We can override aspects of the parent mode here.

  ;; Font-lock support.
  ;;
  ;; We must define the local variable font-lock-defaults to
  ;; declare this mode as one that supports font-lock.
  ;; Then the global-font-lock-mode variable will also concern
  ;; this major mode.
  ;; 
  ;; (`define-derived-mode' apparently does not derive 
  ;; the font-lock settings. Not nice.)
  ;;
  ;; This code taken and adapted from sawfish.el.
  (if (and (boundp 'running-xemacs) (symbol-value 'running-xemacs))
      ;; XEmacs appears to do something like this...
      (put 'symbian-c++-mode 'font-lock-defaults
           (get 'c++-mode 'font-lock-defaults))
    ;; ...with GNU Emacs we need to pull it from `font-lock-defaults-alist'.
    (unless font-lock-defaults
      (set (make-local-variable 'font-lock-defaults)
           (cdr (assoc 'c++-mode font-lock-defaults-alist)))))

  ) ;; end major mode definition

;; Very similar to the "whitesmith" style, so we might want
;; to consider deriving from it, and thus having a shorter
;; definition here.
;;
;; By using the c-lineup-whitesmith-in-block function, we
;; could better deal with code with something like "if (x) {",
;; but there is not supposed to be such code, so why waste
;; processing power dealing with erroneously indented code.
(c-add-style "symbian"
	      '((c-basic-offset . 4)
		(c-comment-only-line-offset . 0)
		(c-offsets-alist

		 ;; function or method
		 ;;
		 ;; opening paren of a function
		 (defun-open . +)
		 (inline-open . +)
		 ;; first line of function body
		 (defun-block-intro . 0) ; was whitesmith
		 ;; closing paren of a function
		 (defun-close . 0) ; was whitesmith

		 ;; statement block
		 ;;
		 ;; opening paren of a statement block
		 (block-open . 0)
		 ;; first line of a statement block
		 (statement-block-intro . 0)
		 ;; closing paren of a statement block
		 (block-close . 0)

		 ;; substatement block
		 ;;
		 (substatement-open . +)
		 (statement-block-intro . 0)
		 (substatement . 0)

		 ;; enum or static array list
		 ;;
		 (brace-list-open . +)
		 (brace-list-intro . 0) ; was whitesmith
		 (brace-list-close . 0) ; was whitesmith
		 
		 ;; class definition
		 ;;
		 (class-open . +)
		 (inclass . 0) ; was whitesmith
		 (class-close . +)

		 ;; an external language block
		 ;;
		 (extern-lang-open . +)
		 (inextern-lang . 0) ; was whitesmith
		 (extern-lang-close . +)
		 
		 ;; a namespace block
		 ;;
		 (namespace-open . +)
		 (innamespace . 0) ; was whitesmith
		 (namespace-close . +)

		 ;; switch case
		 ;;
		 (statement-case-open . +)
		 (statement-case-intro . +)

		 ;; label
		 ;;
		 (label . -)
		 (case-label . 0)

		 ))) ;; end style definition

(add-hook 'symbian-c++-mode-hook
	  (lambda ()
	    (c-set-style "symbian")
	    (setq indent-tabs-mode nil) ; may use tabs for indents
	    (setq tab-width 4) ; typical with Symbian code
	    ;; Convert spaces to tabs so that things are consistent,
	    ;; and don't look messy when tab width is different from
	    ;; that defined above.
	    (add-hook 'local-write-file-hooks 
		      (lambda() 
			(untabify (point-min) (point-max))))

	    ))

(defun symbian-c++-insert-signature ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (insert "// -*- symbian-c++ -*-\n")))

;; For inserting a -*- line into the current buffer.
(define-key symbian-c++-mode-map "\C-cp" 'symbian-c++-insert-signature)
;; Useful when tweaking the indentation rules.
(define-key symbian-c++-mode-map "\C-cs" 'c-show-syntactic-information)

(provide 'symbian-c++-mode)
