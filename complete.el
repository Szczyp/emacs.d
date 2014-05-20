;;; complete.el -- Auto completion

(package-require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)

(global-company-mode 1)

(provide 'complete)
