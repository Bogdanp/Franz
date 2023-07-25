((racket-mode . ((eval . (cl-dolist (s '("match-view"))
                           (put (intern s) 'racket-indent-function #'defun))))))
