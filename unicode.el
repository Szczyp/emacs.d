;; Unicode
(defun unicode-symbol (name)
  (case name
    ('lambda "λ")
    ('set "∈")
    ('composition "∘")
    ('application "⊳")
    ('partial-application "⊵")
    ('f "⨍")))

(defun substitute-unicode (mode table)
  (mapcar #'(lambda (pair) (let ((pattern (car pair))
			    (sym (unicode-symbol (cdr pair))))
			(font-lock-add-keywords
			 mode `((,pattern
				 (0 (progn (compose-region (match-beginning 1) (match-end 1)
							   ,sym 'decompose-region))))))))
	  table))

(provide 'unicode)
