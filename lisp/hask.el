;;; hask.el --- ENGAGE MONAD TRANSFORMERS

(package-require 'haskell-mode)
(package-require 'ghc)
(package-require 'company-ghc)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-to-list 'company-backends 'company-ghc)

(setq company-ghc-show-info t)

(custom-set-variables
 '(haskell-tags-on-save t)
 '(haskell-stylish-on-save t))

(setq haskell-hoogle-command "hoogle -d ~/.hoogle")

(setq haskell-mode-keybindings
      (list (cons (kbd "C-c <RET>") 'haskell-process-load-or-reload)
	    (cons (kbd "C-c t") 'ghc-show-type)
	    (cons (kbd "C-c r") 'haskell-process-restart)
	    (cons (kbd "C-c z") 'haskell-interactive-switch)))

(defun haskell-hook ()
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (ghc-init)

  (define-keys haskell-mode-map haskell-mode-keybindings))

(setq haskell-interactive-mode-keybindings
      (list (cons (kbd "C-c z") 'haskell-interactive-switch-back)
	    (cons (kbd "C-c r") 'haskell-process-restart)))

(defun haskell-interactive-hook ()
  (define-keys haskell-interactive-mode-map haskell-interactive-mode-keybindings))

(defun haskell-cabal-hook ()
  )

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-interactive-mode-hook 'haskell-interactive-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(provide 'hask)
