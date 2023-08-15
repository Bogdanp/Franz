((racket-mode . ((eval . (cl-dolist (s '("let-observable" "update-observable" "λpdate-observable" "match-view"))
                           (put (intern s) 'racket-indent-function #'defun))))))
