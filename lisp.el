;;; lisp.el -- Lisps

(require 'defuns)

(setq lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode shen-mode))

(defun add-lisp-hook (func)
  (add-hooks lisp-modes func))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c <RET>") 'eval-buffer)

;; Highlight sexp under cursor
(package-require 'highlight-parentheses)
(add-lisp-hook 'highlight-parentheses-mode)

;; Lambdas
(defun lambda-as-lambda (mode pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     "λ" 'decompose-region)))))))

;;; Emacs Lisp
(lambda-as-lambda 'emacs-lisp-mode "(\\(\\<lambda\\>\\)")

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

(lambda-as-lambda 'clojure-mode "(\\(\\<fn\\>\\)")

;; Cider
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))

(setq nrepl-hide-special-buffers t)
(setq cider-repl-pop-to-buffer-on-connect nil)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook
	  (lambda () (rebind-evil-tag-navigation clojure-mode-map 'cider-jump 'cider-jump-back)))
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-mode)

;; Run tests in nRepl
(defun nrepl-run-tests (ns)
  (interactive (list (nrepl-current-ns)))
  (save-buffer)
  (nrepl-load-current-buffer)
  (with-current-buffer "*nrepl*"
    (nrepl-send-string
     (format "(clojure.test/run-tests '%s)" ns)
     nrepl-buffer-ns (nrepl-handler (current-buffer)))))
(eval-after-load "clojure-mode"
  '(define-key clojure-mode-map (kbd "C-c C-,") 'nrepl-run-tests))

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

;;; Shen
(package-require 'shen-mode)
(add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))
(eval-after-load "shen-mode"
  '(progn
     (define-key shen-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key shen-mode-map (kbd "C-c C-k")
       (lambda ()
         (interactive)
         (lisp-eval-string (buffer-string))))))
(lambda-as-lambda 'shen-mode "(\\(\\</\\.\\>\\)")


;;; Various inferior lisps

;; Clojure REPL
(defun clojure-repl ()
  (interactive)
  (run-lisp "lein repl"))

;; ClojureScript REPL
(defun clojurescript-repl ()
  (interactive)
  (run-lisp "lein trampoline noderepl"))

;; ClojureScript REPL
(defun clojurescript-rhino-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-rhino"))

;; Shen REPL
(defun shen-repl ()
  (interactive)
  (run-lisp "shen"))

(provide 'lisp)
