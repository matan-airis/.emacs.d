;;
;; Initial Emacs load file
;;

;; Set up ELPA repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Add the given path to the load-path variable.
(defun add-to-load-path (path-string)
  (message (format "Passed %S..." path-string))
  (if (stringp path-string)
      (when (file-exists-p path-string)
        (message (format "Adding %S to load-path..." path-string))
        (add-to-list 'load-path (expand-file-name path-string)))
    (crs-add-to-load-path (car path-string))
    (if (cdr path-string)
        (crs-add-to-load-path (cdr path-string)))))

(add-to-load-path (expand-file-name "~/.emacs.d/lisp"))

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(require 'settings)
(require 'quick-yes)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-term-color-vector
   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"] t)
 '(bell-volume 0)
 '(blink-cursor-mode nil)
 '(blink-matching-paren t)
 '(compilation-scroll-output t)
 '(custom-safe-themes
   '("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "854dc57026d3226addcc46b2b460034a74609edbd9c14e626769ac724b10fcf5" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "d26c1e1b5497c2118820d70455652681a8776df23c2bc202ab4d3c9a8171b9d4" default))
 '(efs-use-passive-mode t)
 '(fci-rule-character-color "#202020")
 '(font-lock-maximum-size 400000)
 '(frame-brackground-mode 'dark)
 '(fringe-mode 10 nil (fringe))
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(linum-format " %6d ")
 '(load-home-init-file t t)
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(main-line-separator-style 'chamfer)
 '(nyan-wavy-trail t)
 '(org-agenda-files '("~/org/agenda/tasks.org") t)
 '(package-selected-packages
   '(dynamic-graphs graphviz-dot-mode nyan-mode diminish powerline ggtags ivy bison-mode kotlin-mode pdf-tools company-lsp cargo flycheck-rust flymake-rust racer rust-mode prettier-js flycheck flycheck-flow add-node-modules-path web-mode org org-download org-notebook org-pomodoro auctex auctex-latexmk company-bibtex company-reftex magit-todos magithub sphinx-doc projectile projectile-codesearch projectile-speedbar forge magit eldoro ereader ivy-bibtex timesheet twittering-mode lua-mode string-inflection company-auctex company-quickhelp company-math elpy opencl-mode auto-virtualenvwrapper ein langtool latex-pretty-symbols latex-preview-pane prolog py-autopep8 py-isort py-smart-operator python-docstring python-pep8 virtualenvwrapper sml-mode zenburn-theme tuareg rainbow-mode python-mode merlin latex-unicode-math-mode latex-math-preview latex-extra language-detection))
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F")
 '(standard-indent 4)
 '(truncate-lines t)
 '(truncate-partial-width-windows t))

;; initialize packages
(package-initialize)

;; Make Emacs split horizontally by default (i.e when doing grep/completion/C-h b)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Turn off bell alarms
(setq ring-bell-function 'ignore)

;; Turn off welcome screen
(setq inhibit-startup-message t)

;; Set a nice scratch message
(setq initial-scratch-message ";; welcome, h4x0r.

")

;; load theme
(load-theme 'zenburn t)
(enable-theme 'zenburn)

;; Load keys the last, in order to override bad key bindings
(require 'keys)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
