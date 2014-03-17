;;; haskell.el --- ENGAGE MONAD TRANSFORMERS

(package-require 'haskell-mode)
(package-require 'ghc)

(custom-set-variables
 '(haskell-tags-on-save t)
 '(haskell-stylish-on-save t))

(setq haskell-process-type 'cabal-repl)

;; auto-complete source using ghc-doc
(defun ac-haskell-candidates ()
  (let ((pattern (buffer-substring (ghc-completion-start-point) (point)))
        (symbols (ghc-select-completion-symbol)))
    (all-completions pattern symbols)))

;; Setup auto-complete for haskell-mode
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'haskell-mode)
     (ac-define-source ghc
       '((candidates . ac-haskell-candidates)))))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(defun haskell-hook ()
  (turn-on-haskell-indentation)
  ;; (turn-on-haskell-doc-mode)
  (ghc-init)
  (add-to-list 'ac-sources 'ac-source-ghc)

  (define-key haskell-mode-map (kbd "C-c <RET>") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c i") 'ghc-show-info-popup)
  (define-key haskell-mode-map (kbd "C-c t") 'ghc-show-type)
  (define-key haskell-mode-map (kbd "C-c r") 'haskell-process-restart))

(defun haskell-cabal-hook ()
  )

;; Put ghc-show-info in a popup
(package-require 'popup)
(defun ghc-show-info-popup ()
  (interactive)
  (popup-tip (ghc-get-info (ghc-things-at-point))
             :around t :scroll-bar t))


;;; Idris (for want of a better place to put it)
(package-require 'idris-mode)
(add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))


(provide 'haskell)
