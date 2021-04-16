;; Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'js2-mode)
(straight-use-package 'helm-ag)
(straight-use-package 'projectile)
(straight-use-package 'helm-projectile)
(straight-use-package 'web-mode)
(straight-use-package 'ws-butler)
(straight-use-package 'zenburn-theme)
(straight-use-package 'typescript-mode)
(straight-use-package 'undo-tree)
(straight-use-package 'go-mode)
(straight-use-package 'protobuf-mode)
(straight-use-package 'magit)
(straight-use-package 'flycheck)
(straight-use-package 'tide)
(straight-use-package 'lsp-mode)
;; (straight-use-package 'lsp-ui)
(straight-use-package 'company)
;; (straight-use-package 'company-go)
;; (straight-use-package 'go-guru)

;; Requires for ill-behaved packages

(require 'helm-config)
(require 'helm-projectile)
(require 'web-mode)
(require 'ws-butler)
(require 'lsp-mode)
;; (require 'go-guru)
;; Helper funs

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  "Indent sole parenthesis in loca's way."
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; From https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Global config

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))
(setq projectile-globally-ignored-directories
      (append '(".git" "node_modules" "bower_components" "dist") projectile-globally-ignored-directories))
(setq helm-split-window-in-side-p t)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
  ;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Global modes
(helm-mode 1)
(projectile-mode +1)
(helm-projectile-on)
(projectile-global-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(global-undo-tree-mode)
(load-theme 'zenburn t)
(column-number-mode 1)

;; Key bindings

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-w") #'xah-cut-line-or-region)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-F") 'helm-projectile-ag)
(global-set-key (kbd "M-F") 'helm-projectile-find-file)
(global-set-key (kbd "<f1>")  'move-line-up)
(global-set-key (kbd "<f2>")  'move-line-down)
(global-set-key (kbd "￿") 'undo-tree-redo)

;; Ruby

(setq ruby-align-chained-calls nil)
(setq ruby-align-to-stmt-keywords nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-deep-arglist nil)
(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-pair-mode)))
(add-hook 'ruby-mode-hook 'ws-butler-mode)
(add-to-list 'auto-mode-alist '("\\.hamlbars$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; CSS

(setq scss-compile-at-save nil)
(setq css-indent-offset 2)

;; JS / TS

(setq js-indent-level 2)
(add-hook 'js-mode-hook 'ws-butler-mode)
(add-hook 'js3-mode-hook 'ws-butler-mode)
(setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
(setq js3-enter-indents-newline t) ; don't need to push tab before typing
(setq js3-indent-on-enter-key t)   ; fix indenting before moving on
(setq typescript-indent-level 2)
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (company-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
  (setq-local web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset 2) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset 2) ; web-mode, js code in html file
)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)
			)
		  )
	  )


(if (getenv "CORE")
    (progn
       ;; enable typescript-tslint checker
	(add-hook 'flycheck-mode-hook
        (lambda () (flycheck-add-mode 'typescript-tslint 'web-mode)))
       (custom-set-variables
       '(flycheck-typescript-tslint-executable (concat (getenv "CORE") "/frontend/admin/node_modules/tslint/bin/tslint")))
       (custom-set-variables
       '(flycheck-typescript-tslint-config (concat (getenv "CORE") "/frontend/admin/tslint.json")))
	    )
)

;; Go

(defun custom-go-mode-hook ()
  (setq tab-width 2 indent-tabs-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq gofmt-args (list "-local" "github.com/DevotedHealth"))
  ;; disable inline documentation
  (setq lsp-ui-sideline-enable nil)
  ;; disable showing docs on hover at the top of the window
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-imenu-kind-position 'top))
(add-hook 'go-mode-hook 'custom-go-mode-hook)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Proto

(require 'protobuf-mode)
(defun setup-proto-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'protobuf-mode-hook #'setup-proto-mode)

;; Misc

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq gc-cons-threshold 500000000)
(setq lsp-enable-file-watchers nil)
(setq lsp-enable-links nil)
(setq lsp-idle-delay 1.25)
