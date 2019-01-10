(defun l () 
    (load "main.lisp"))

(setf *house-factors 
    '(cheap near-shops near-uni))

(defun make-perceptron (factors-list)
    (let ((weighted-list nil))
        (dolist (element factors-list (reverse (push (list 'bias 1) weighted-list)))
            (push (list element 1) weighted-list))))

(defun make-house (factors-list weights-list)
    (let ((house nil))
        (dolist (element factors-list (reverse house))
            (setf house (push (list element (first weights-list)) house) weights-list (rest weights-list)))))