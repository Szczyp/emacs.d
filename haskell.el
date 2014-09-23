;;; haskell.el --- ENGAGE MONAD TRANSFORMERS

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

(defun haskell-hook ()
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (ghc-init)

  (define-key haskell-mode-map (kbd "C-c <RET>") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c t") 'ghc-show-type)
  (define-key haskell-mode-map (kbd "C-c r") 'haskell-process-restart))

(defun haskell-cabal-hook ()
  )

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(provide 'haskell)
