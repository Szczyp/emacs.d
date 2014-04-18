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

;;; Emacs Lisp
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

;;; Clojure
(package-require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))

(substitute-unicode 'clojure-mode
		    (list (cons "(\\(\\<fn\\>\\)" 'lambda)
			  (cons "(\\(\\<comp\\>\\)" 'composition)
			  (cons "(\\(\\<apply\\>\\)" 'application)
			  (cons "(\\(\\<partial\\>\\)" 'partial-application)
			  (cons "\\(#\\){" 'set)
			  (cons "\\(#\\)(" 'f)))

;; Cider
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)

(defun cider-eval-defun-at-point-or-region ()
  (interactive)
  (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
    (cider-eval-defun-at-point)))

(defun spawn-chrome ()
  (interactive)
  (start-process "chrome" nil "chromium" "localhost:8080"))

(add-hook 'clojure-mode-hook
	  (lambda () (progn
		  (define-key clojure-mode-map (kbd "C-c j") 'cider-jack-in)
		  (define-key clojure-mode-map (kbd "C-c b") 'spawn-chrome))))

(add-hook 'cider-mode-hook
	  (lambda () (progn
		  (cider-turn-on-eldoc-mode)
		  (rebind-evil-tag-navigation cider-mode-map 'cider-jump 'cider-jump-back)
		  (define-key cider-mode-map (kbd "C-c r") 'cider-restart)
		  (define-key cider-mode-map (kbd "C-c <RET>") 'cider-eval-buffer)
		  (define-key cider-mode-map (kbd "C-c <SPC>") 'cider-eval-defun-at-point-or-region))))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-mode)

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

;; Cljsbuild
(package-require 'cljsbuild-mode)

(provide 'lisp)
