;; The Key bindings

;;Set hippie-expand on c-tab
;;(global-set-key (kbd "C-<tab>") 'hippie-expand)

;;Set C-z to unset
(global-unset-key [?\C-z])


;;Set switch
(global-set-key [?\C-.] 'wcy-switch-buffer-forward)
(global-set-key [?\C-,] 'wcy-switch-buffer-backward)
(global-set-key [?\C-<] 'wcy-switch-buffer-backward)

;;Set ountline-minor-mode prefix to C-o
(setq outline-minor-mode-prefix [(control o)]) 
(global-set-key [(meta o)] 'open-line)

;;Rebind 'C-x C-b' to buffer-menu
(global-set-key "\C-x\C-b" 'buffer-menu)

;;Set debug key for gdb
(add-hook 'gdb-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "<f10>") 'gud-next)
             (define-key c-mode-base-map (kbd "<f11>") 'gud-step)
             (define-key c-mode-base-map (kbd "<f5>")  'gud-go)
             (define-key c-mode-base-map (kbd "<f6>")  'gud-cont)
             (define-key c-mode-base-map (kbd "<f9>")  'gud-break)
             (define-key c-mode-base-map (kbd "S-<f9>") 'gud-remove)))  