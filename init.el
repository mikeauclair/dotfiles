(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

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

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-w") #'xah-cut-line-or-region)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-globally-ignored-directories
      (append '(".git" "node_modules" "bower_components") projectile-globally-ignored-directories))
(projectile-global-mode t)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-S-f") 'helm-projectile-ag)
(global-set-key [(super shift f)] 'helm-projectile-find-file)
(setq-default indent-tabs-mode nil)
(setq ruby-align-chained-calls nil)
(setq ruby-align-to-stmt-keywords nil)
(setq ruby-deep-indent-paren nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-deep-arglist nil)
(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-pair-mode)))
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(add-to-list 'auto-mode-alist '("\\.hamlbars$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

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
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
(setq js3-enter-indents-newline t) ; don't need to push tab before typing
(setq js3-indent-on-enter-key t)   ; fix indenting before moving on
(require 'ws-butler)
(add-hook 'ruby-mode-hook 'ws-butler-mode)
(add-hook 'js-mode-hook 'ws-butler-mode)
(add-hook 'js3-mode-hook 'ws-butler-mode)
(load-theme 'zenburn t)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
