(define-key global-map (kbd "RET") 'newline-and-indent)

(package-require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(package-require 'projectile)
(projectile-global-mode)

(package-require 'ag)

(provide 'global)
