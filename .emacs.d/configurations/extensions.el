;;extension file

;; IMPORTANT! to prevent cedet hang emcas when open a file in Python Mode
(eval-after-load "wisent-python"
  (remove-hook 'python-mode-hook 'wisent-python-default-setup))

;; Load Emacs Code Browser which NEED cedet be loaded first
;; (require 'ecb-autoloads)

;;ascii table
(require 'ascii)

;;chinese calender
(require 'chinese-calendar)

;; TRAMP
(require 'tramp)
(setq tramp-default-method "scp")
(setq tramp-default-user "chenweiyang")
(add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp nil))
;(add-to-list 'bkup-backup-directory-info
;             (list "." "~/.emacs.d/backups/" 'full-path))
;(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)
(setq tramp-bkup-backup-directory-info nil)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -f %s/TAGS -R %s" path-to-ctags dir-name (directory-file-name dir-name)))
)

;; w3m
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(defun w3m-browse-current-buffer ()
  (interactive)
    (let ((filename (concat (make-temp-file "w3m-") ".html")))
      (unwind-protect
           (progn
             (write-region (point-min) (point-max) filename)
             (w3m-find-file filename))
        (delete-file filename))))

(add-hook 'dired-mode-hook
          (lambda ()
	       (define-key dired-mode-map "\C-xm" 'dired-w3m-find-file)))
(defun dired-w3m-find-file ()
  (interactive)
  (require 'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))
