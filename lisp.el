;;; lisp.el -- Lisps

(require 'defuns)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

(define-key lisp-mode-shared-map (kbd "C-c <SPC>") 'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-c <RET>") 'eval-buffer)

;; Highlight sexp under cursor
(package-require 'highlight-parentheses)
(add-lisp-hook 'highlight-parentheses-mode)

;; Emacs Lisp
(dolist (c (string-to-list ":_-?!#*"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

(substitute-unicode 'emacs-lisp-mode
		    (list (cons "(\\(\\<lambda\\>\\)" 'lambda)))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (rebind-evil-tag-navigation emacs-lisp-mode-map 'find-function-at-point nil)))

;; Clojure
(package-require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.hl?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot?$" . clojure-mode))

;; Monads
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (progn
	       (put-clojure-indent 'mlet 1)
	       (put-clojure-indent 'mdo 0))))

(font-lock-add-keywords
 'clojure-mode
 '(("mlet" . font-lock-keyword-face)
   ("mdo" . font-lock-keyword-face)
   ("return" . font-lock-keyword-face)))

;; Unicode
(substitute-unicode 'clojure-mode
		    (list (cons "\\b\\(\\<fn\\>\\)\\b" 'lambda)
			  (cons "\\b\\(\\<comp\\>\\)\\b" 'composition)
			  (cons "\\b\\(\\<apply\\>\\)\\b" 'application)
			  (cons "\\b\\(\\<partial\\>\\)\\b" 'partial-application)
			  (cons "\\(#\\){" 'set)
			  (cons "\\(#\\)(" 'f)))

;; Cider
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))

(setq cider-repl-use-clojure-font-lock t)
(setq cider-font-lock-as-clojure t)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)

(defun cider-eval-defun-at-point-or-region ()
  (interactive)
  (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
    (cider-eval-defun-at-point)))

(defun define-keys (mode table)
  (mapcar #'(lambda (pair) (let ((key (car pair))
			    (f (cdr pair)))
			(define-key mode key f)))
	  table))

(setq clojure-mode-keybindings (list (cons (kbd "C-c j") 'cider-jack-in)
				     (cons (kbd "C-c b") 'spawn-chrome)))

(add-hook 'clojure-mode-hook
	  (lambda () (define-keys clojure-mode-map clojure-mode-keybindings)))

(setq cider-mode-keybindings
      (list (cons (kbd "C-c r") 'cider-restart)
	    (cons (kbd "C-c n") 'cider-repl-set-ns)
	    (cons (kbd "C-c z") 'cider-switch-to-relevant-repl-buffer)
	    (cons (kbd "C-c <RET>") 'cider-eval-buffer)
	    (cons (kbd "C-c <SPC>") 'cider-eval-defun-at-point-or-region)))

(add-hook 'cider-mode-hook
	  (lambda () (progn
		  (cider-turn-on-eldoc-mode)
		  (rebind-evil-tag-navigation cider-mode-map 'cider-jump 'cider-jump-back)
		  (define-keys cider-mode-map cider-mode-keybindings))))

(setq cider-repl-mode-keybindings
      (list (cons (kbd "C-c z") 'cider-switch-to-last-clojure-buffer)))

(add-hook 'cider-repl-mode-hook
	  (lambda () (progn
		  (paredit-mode)
		  (rainbow-mode)
		  (define-keys cider-repl-mode-map cider-repl-mode-keybindings))))

;;Kibit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun cljx ()
  "Run lein cljx auto"
  (interactive)
  (async-shell-command "lein cljx auto" "*cljxbuild*"))

(defun spawn-chrome ()
  (interactive)
  (start-process "chrome" nil "chromium" "localhost:8080"))

;; Cljsbuild
(package-require 'cljsbuild-mode)
(setq cljsbuild-verbose t)
(setq cljsbuild-show-buffer-on-failure t)
(setq cljsbuild-show-buffer-on-warnings t)

(provide 'lisp)
