(defun l () 
    (load "main.lisp"))

(setf *house-factors 
    '(cheap near-shops near-uni))

(defun make-perceptron (l)
    (let ((weighted-list nil))
        (dolist (element l (reverse (push (list 'bias 1) weighted-list)))
            (push (list element 1) weighted-list))))