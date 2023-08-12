((racket-mode . ((eval . (cl-dolist (s '("let-observable" "update-observable" "match-view"))
                           (put (intern s) 'racket-indent-function #'defun))))))
