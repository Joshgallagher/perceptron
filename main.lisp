(defun l () 
    (load "main.lisp"))

(setf *house-factors 
    '(cheap near-shops near-uni))

(defun make-perceptron (factors-list)
    (let ((weighted-list nil))
        (dolist (element factors-list (reverse (push (list 'bias 1) weighted-list)))
            (push (list element 1) weighted-list))))

(defun make-house (factors-list weights-list)
    ;; Set local var called house to nil
    (let ((house nil))
        ;; Iterate over factors-list
        ;; Return item from factors-list as element
        ;; Reverse the returned list set to house
        (dolist (element factors-list (reverse house))
            ;; Get the first element from the weights-list
            ;; and create a list with the 'element' from factors-list
            ;;
            ;; On each iteration, call rest on weights-list and assign to self
            ;; Iteration 1: weights-list with rest -> (0 1)
            ;; Iteration 2: weights-list with rest -> (1)
            ;; Iteration 3: weights-list with rest -> nil
            ;;
            ;; Each iteration a key/value pair is created and added to 'house'
            ;; The value in the key/value pair is then removed from weights-list
            ;; with the rest operator
            (setf house (push (list element (first weights-list)) house) 
                weights-list (rest weights-list)))))

(defun score (perceptron house)
    ;; Set a local var to hold the overall score
    (let ((score 0))
        ;; Iterate ovet the house list
        ;; Returns -> (cheap 1)
        (dolist (var house)
            ;; var-val has (cheap 1), and
            ;; retrieves 1 from this list
            (let ((var-val (second var))
                ;; var-weight has (cheap 1), uses first to get 'cheap',
                ;; then uses assoc to find this symbol in the perceptron list
                ;; Assoc returns (cheap 1) and second gets the value 1 from this
                (var-weight (second (assoc (first var) perceptron))))
            ;; var-val * var-weight is added to score
            (incf score (* var-val var-weight))))
        ;; Bias value is retrieved and added to score
        (incf score (second (assoc 'bias perceptron)))))

;; 
;; Setup and run perceptron
;;

(setf *perceptron 
    (make-perceptron *house-factors))

(setf *house 
    (make-house *house-factors '(1 0 1)))

(print (score *perceptron *house))
