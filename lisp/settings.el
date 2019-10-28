;;
;; Colors settings and other settings
;;

(require 'custom-functions)

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil)
)

;; Run "xlsfonts | grep courier-medium-r" to view available fonts
;; For this font, you should install on your machine the the package xfonts-75dpi.
;(set-default-font "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1")
(set-frame-font "DejaVu Sans Mono-14")

;; Set the default font for new frames. This is helpful when doing new-frame[-on-display] or when running emacsclient.
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-14"))

;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "orangered" "green4" "yellow4"
       "deepskyblue" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Get rid of toolbar
(tool-bar-mode nil)

(setq grep-find-history '("find . -type f -print0 | xargs -0 -e grep -n "))
(setq lazy-highlight-initial-delay '0)

(line-number-mode t)
(column-number-mode t)

;; Set C-x c to quit, not C-x C-c
(global-set-key [(control x) (control c)] (defun dont-kill-emacs() (interactive) (message "Use C-x c to leave emacs")))
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

;; Don't ask before killing a buffer with a running process.
;; This list holds hooks to run before killing a buffer, and it has originally contained only process-kill-buffer-query-function.
;; In order to restore the default, remove this line or replace it with: (setq kill-buffer-query-functions 'process-kill-buffer-query-function)
(setq kill-buffer-query-functions nil)

;; Set google-chrome as the default browser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Set the C-mode's tab to 4 spaces.
(setq c-basic-offset 4)

;; When in text mode, proper line size should be 80.
(setq-default fill-column 80)

;; Indent enumerate, itemize properly.
(eval-after-load "latex"
  '(add-to-list 'LaTeX-indent-environment-list
                '("itemize" LaTeX-indent-item)))
(eval-after-load "latex"
  '(add-to-list 'LaTeX-indent-environment-list
                '("enumerate" LaTeX-indent-item)))
(eval-after-load "latex"
  '(add-to-list 'LaTeX-indent-environment-list
                '("description" LaTeX-indent-item)))

(setq LaTeX-indent-level 2)
(setq LaTeX-indent-level-item-continuation 8)

(eval-after-load "latex"
  '(add-to-list 'LaTeX-verbatim-environments "inline"))

(eval-after-load "latex"
  '(add-to-list 'LaTeX-verbatim-environments "thapl"))

(setq bibtex-text-indentation 2)
(setq bibtex-contline-indentation 4)
(setq bibtex-entry-offset 0)
(setq bibtex-align-at-equal-sign nil)
(setq bibtex-entry-format '(numerical-fields page-dashes
last-comma delimiters unify-case))
;; Do AucTeX things

(setq-default TeX-auto-save t)
(setq-default TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

;; UTF-8 forever
(prefer-coding-system 'utf-8)

;; Python
(elpy-enable)
(setq elpy-rpc-python-command "python3")

(eval-after-load "elpy"
  '(company-quickhelp-mode)
  )

;; Look for a .venv file
(defun pyvenv-autoload ()
  (require 'projectile)
  (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
    (if (file-exists-p pfile)
        (pyvenv-activate (with-temp-buffer
                         (insert-file-contents pfile)
                         (nth 0 (split-string (buffer-string))))))))
(add-hook 'python-mode-hook 'pyvenv-autoload)

(provide 'settings)
