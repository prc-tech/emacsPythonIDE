;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Setup some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    material-theme                  ;; Theme
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ====================================
;; Basic Customization
;; ====================================

(setq inhibit-startup-message t)  ;; Hide the startup message
(load-theme 'material t)          ;; Load material theme
(column-number-mode)         ; Enable column number modes

;; (global-linum-mode t)             ;; Enable line numbers globally
(global-display-line-numbers-mode t) ; Enable line numbers globally
; Disbale line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Allow hash to be entered  
(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))


;; ====================================
;; DEVELOPMENT SETUP
;; ====================================
;; Enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; User-Defined init.el ends here
