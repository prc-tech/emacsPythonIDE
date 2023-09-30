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
    ein                             ;; Emacs IPython Notebook
    flycheck                        ;; On the fly syntax checking
   ; py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black format on save
    magit                           ;; Git integration
    material-theme                  ;; Theme
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
;;-------------end of package setup---------------



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
;;------------end of basic customization-----------



;; ====================================
;; DEVELOPMENT SETUP
;; ====================================
;;
;; Enable elpy - emacs Lisp python interpreter
(elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "consol --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
	     "jupyter")

;; Enable Flycheck - real time syntax checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 ---> using blacken instead
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;------------end of Development setup-----------



;; User-Defined init.el ends here
