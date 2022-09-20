;;
;; Packages, colors, and other settings
;;
(require 'custom-functions)

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil)
)

;; Run "xlsfonts | grep courier-medium-r" to view available fonts For this font,
;; you should install on your machine the the package xfonts-75dpi.
(set-frame-font "DejaVu Sans Mono-13")

;; Set the default font for new frames. This is helpful when doing
;; new-frame[-on-display] or when running emacsclient.
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-13"))

;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "orangered" "green4" "yellow4"
       "deepskyblue" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Get rid of toolbar
(tool-bar-mode -1)

(setq grep-find-history '("find . -type f -print0 | xargs -0 -e grep -n "))
(setq lazy-highlight-initial-delay '0)

;; Some visual things
(line-number-mode t)
(column-number-mode t)

;; Always font lock
(font-lock-mode)

;; Set C-x c to quit, not C-x C-c
(global-set-key [(control x) (control c)]
                (defun dont-kill-emacs() (interactive)
                       (message "Use C-x c to leave emacs")))
(global-set-key [(control x) c] 'save-buffers-kill-terminal)
(global-set-key "\C-xc" 'save-buffers-kill-terminal)

;; Colors displayed by diff mode.
(defun update-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "darkgreen")
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "darkred")
  (set-face-attribute 'diff-changed nil
                      :foreground "white" :background "purple"))
(eval-after-load "diff-mode"
  '(update-diff-colors))

;; Don't ask before killing a buffer with a running process.  This list holds
;; hooks to run before killing a buffer, and it has originally contained only
;; process-kill-buffer-query-function.  In order to restore the default, remove
;; this line or replace it with: (setq kill-buffer-query-functions
;; 'process-kill-buffer-query-function)
(setq kill-buffer-query-functions nil)

;; Set firefox as the default browser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; When in text mode, proper line size should be 80.
(setq-default fill-column 80)

;; Look for passwords in .authinfo.gpg
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; LaTeX settings
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :functions font-latex-add-keywords
  :bind
  ("C-c /" . latex-close-block)
  :hook ((plain-TeX-mode-hook . (lambda ()
                                  (set (make-local-variable 'TeX-electric-math)
                                       (cons "$" "$"))))
         (LaTeX-mode-hook . (lambda () (set (make-local-variable 'TeX-electric-math)
                                            (cons "\\(" "\\)"))))
         (LaTeX-mode-hook . visual-line-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . LaTeX-math-mode)
         (LaTeX-mode-hook . turn-on-reftex)
         (LaTeX-mode-hook . turn-on-auto-fill)
         (LaTeX-mode-hook . autoref-latex-mode-hook))
  :config
  (add-to-list 'LaTeX-indent-environment-list
               '("itemize" LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list
               '("enumerate" LaTeX-indent-item))
  (add-to-list 'LaTeX-indent-environment-list
               '("description" LaTeX-indent-item))
  (defun autoref-latex-mode-hook ()
    (font-latex-add-keywords '(("autoref" "*{") ("Autoref" "{") ("nameref" "*{"))
                             'reference))
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))
  (setq LaTeX-indent-level 2
        LaTeX-indent-level-item-continuation 8
        bibtex-text-indentation 2
        bibtex-contline-indentation 4
        bibtex-entry-offset 0
        bibtex-align-at-equal-sign nil
        bibtex-entry-format '(numerical-fields
                              page-dashes
                              last-comma
                              delimiters
                              unify-case))
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil
                TeX-PDF-mode t
                TeX-engine 'xetex
                TeX-source-correlate-mode t
                TeX-view-program-selection (quote ((engine-omega "dvips and gv")
                                                   (output-dvi "xdvi")
                                                   (output-pdf "Okular")
                                                   (output-html "xdg-open"))))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

;; Make symbols pretty in LaTeX
(use-package latex-pretty-symbols
  :ensure t)

(defun my-auctex-latexmk-advice (req feature &rest args)
  "Call REQ with FEATURE and ARGS, unless FEATURE is `tex-buf'."
  (unless (eq feature 'tex-buf)
    (apply req feature args)))

;; Add compilation with latexmk to AucTeX
(use-package auctex-latexmk
  :ensure t
  :after latex
  :init
  (advice-add 'require :around #'my-auctex-latexmk-advice)
  (auctex-latexmk-setup)
  (advice-remove 'require #'my-auctex-latexmk-advice)
  )

;; UTF-8 forever
(prefer-coding-system 'utf-8)

;; Python
(use-package elpy
  :ensure t
  :defer t
  :after (flycheck company)
  :hook (elpy-mode-hook . flycheck-mode)
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq elpy-rpc-python-command "python3")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; projectile
(use-package projectile
  :ensure t
  :bind ((:map projectile-mode-map
               ("C-c p" . projectile-command-map)))
  :demand t ; We need projectile immediatly
  :init
  (setq projectile-mode-line-prefix " 🚧")
  :config

  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)

  ;; ignore some common files for projectile
  (setq projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))

  ;; include the project root directory in projectile-find-dir list
  (setq projectile-find-dir-includes-top-level t)

  ;; http://emacs.stackexchange.com/a/10187/115
  (defun modi/kill-non-project-buffers (&optional kill-special)
    "Kill buffers that do not belong to a `projectile' project.
With prefix argument (`C-u'), also kill the special buffers."
    (interactive "P")
    (let ((bufs (buffer-list (selected-frame))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (let ((buf-name (buffer-name buf)))
            (when (or (null (projectile-project-p))
                      (and kill-special
                           (string-match "^\*" buf-name)))
              ;; Preserve buffers with names starting with *scratch or *Messages
              (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                (message "Killing buffer %s" buf-name)
                (kill-buffer buf))))))))

  (defun modi/projectile-find-file-literally (&optional arg)
    "Jump to a project's file literally (see `find-file-literally') using
completion.  With a prefix ARG invalidates the cache first.
Using this function over `projectile-find-file' is useful for opening files that
are slow to open because of their major mode. `find-file-literally' always opens
files in Fundamental mode."
    (interactive "P")
    (projectile-maybe-invalidate-cache arg)
    (projectile-completing-read
     "Find file literally: "
     (projectile-current-project-files)
     :action `(lambda (file)
                (find-file-literally (expand-file-name file ,(projectile-project-root)))
                (run-hooks 'projectile-find-file-hook))))

  (bind-keys
   ("C-c p K" . modi/kill-non-project-buffers)
   ("C-c p r" . projectile-replace-regexp)
   ("C-c s m" . modi/projectile-switch-project-magit-status))


  (defun modi/projectile-switch-project-magit-status ()
    "Switch to other project and open Magit status there."
    (interactive)
    (let ((projectile-switch-project-action #'magit-status))
      (call-interactively #'projectile-switch-project)))

  (projectile-mode))

(use-package find-file-in-project
  :ensure t)

;; clang-format
(use-package clang-format+
  :ensure t
  :hook (c-mode-common-hook . clang-format+-mode))

;; google c++ style
(use-package google-c-style
  :ensure t
  :hook (c-mode-common-hook . google-set-c-style))

;; company mode (auto completions)
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-tooltip-align-annotations t
        company-show-quick-access t
        company-minimum-prefix-length 1
        company-idle-delay 0.0)
  :config (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

;; AI autocompleters
(use-package company-tabnine
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))

;; TypeScript setup
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . flycheck-mode)
         (before-save . tide-format-before-save)))

;; React/Node.js/Vue settings
(use-package web-mode
  :ensure t
  :diminish "🌎"
  :mode
  (("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jst.ejs\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.jsx?$" . web-mode)
  ("\\.tsx\\'" . web-mode))
  :init
  (setq web-mode-enable-block-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-comment-style 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package vue-mode
  :ensure t
  :mode (("\\.vue\\'" . vue-mode)))

;; lsp stuff (mostly latex but not only)
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands lsp lsp-deferred
  :hook ((lsp-after-open . lsp-enable-imenu)
         (LaTeX-mode-hook . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-after-open . (lambda ()
                            (setq-local company-minimum-prefix-length 1
                                  company-idle-delay 0.0) ;; default is 0.2
                            ))


         )
  :bind (:map lsp-mode-map
              ("C-c C-t" . lsp-describe-thing-at-point))
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-clients-texlab-executable "~/.cargo/bin/texlab"
        lsp-go-gopls-server-path "~/gopath/bin/gopls"
        lsp-completion-provider :capf))

;; Lua configuration
(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :config
    (setq lua-indent-level 4)
    (setq lua-indent-string-contents t))

;; activate nyan-mode (nyan cat location indicator in the modeline)
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t)
  (nyan-start-animation))

;; activate rainbow mode (look for colors and show them nicely)
(use-package rainbow-mode
  :ensure t
  :hook prog-mode
  :diminish rainbow-mode)

;; Whitespace mode: Mark columns that go past 80
;; But actually, don't enable in magit buffers (consider giving up on global?)
(defun prevent-whitespace-mode-for-magit ()
  (not (derived-mode-p 'magit-mode)))
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)

;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Kill fucking annoying eldoc mode which jumps to definition when I don't want it
(global-eldoc-mode -1)

;; Syntax highlight Cap'n Proto
(use-package capnp-mode
  :ensure t
  :mode "\\.capnp\\'")

;; Syntax highlighting and stuff for golang
(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use
         ;; lsp-mode/gopls instead uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (setq whitespace-style '(face empty trailing)
        tab-width 4
        indent-tabs-mode 1)
  )


;; Org-mode stuff
(setq org-agenda-files '("~/org/agenda"))
(setq org-agenda-start-on-weekday 0)
(setq org-columns-default-format
      "%60ITEM(Task) %8Effort(Estim){:} %40DEADLINE(Deadline) %40SCHEDULED(Schedule)")

;; Ivy and amx
(use-package amx
  :ensure t
  :after ivy
  :config (amx-mode))

(use-package ivy
  :ensure t
  :bind (("C-c u" . ivy-resume))
  :diminish ivy-mode
  :demand t ; load immediatly
  :config
  (ivy-mode)

  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-virtual-abbreviate 'full ; Show the full virtual file paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-selectable-prompt t)

  ;; modify default search behaviour of ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press)))

;; Better experience with icons for ivy
;; https://github.com/seagle0128/all-the-icons-ivy-rich/
(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy ivy-rich)
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-icon-size 0.8))

;; More friendly interface for ivy
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :ensure t
  :after (ivy)
  :hook (counsel-mode . ivy-rich-mode)
  :config
  ;; For better performance
  ;; Better experience with icons
  (setq ivy-rich-parse-remote-buffer nil))

;; counsel (stuff for ivy)
(use-package counsel
  :ensure t
  :after ivy
  :diminish counsel-mode
  :bind
  ("C-x C-f" . counsel-find-file)
  ("s-g" . counsel-git-grep)
  :config
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :hook counsel-mode)

;; powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Coq/iris
(use-package proof-general
  :mode ("\\.v'" . coq-mode)
  :ensure t
  :init
  (dolist (var (car (read-from-string
                     (shell-command-to-string
                      "~/.local/bin/opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq coq-prog-name "/home/mip/.opam/default/bin/coqtop"))

(use-package company-coq
  :ensure t
  :after company proof-general)

;; magit
(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit
  :bind ((:map forge-issue-section-map
               ("C-c C-v" . forge-browse-topic))
         (:map forge-pullreq-section-map
               ("C-c C-v" . forge-browse-topic))))

;; string inflections (camelize stuff)
(use-package string-inflection
  :ensure t
  :bind (:map prog-mode-map
              ("C-c C-u" . string-inflection-all-cycle)))

;; Remove stuff I always use from the modeline
(use-package diminish
  :ensure t
  :config
  (diminish 'global-whitespace-mode)
  (diminish 'auto-revert-mode)
  (diminish 'auto-fill-function)
  (diminish 'visual-line-mode))

(provide 'settings)
