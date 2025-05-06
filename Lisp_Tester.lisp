(defun test-equal (a b)
  (if (equal a b) 'PASS 'FAIL))

(defun run-tests ()
  (let ((results
         (list
          ;; set-member
          (cons 'set-member (test-equal (set-member '(1 2 3) 2) t))
          (cons 'set-member-miss (test-equal (set-member '(1 2 3) 4) nil))

          ;; set-union
          (cons 'set-union (test-equal (set-union '(1 2) '(2 3)) '(3 1 2)))

          ;; set-intersection
          (cons 'set-intersection (test-equal (set-intersection '(1 2 3) '(2 3 4)) '(2 3)))

          ;; set-diff
          (cons 'set-diff (test-equal (set-diff '(1 2 3) '(2 4)) '(1 3)))

          ;; boolean-xor
          (cons 'boolean-xor (test-equal (boolean-xor t nil) t))

          ;; boolean-implies
          (cons 'boolean-implies (test-equal (boolean-implies t nil) nil))

          ;; boolean-eval
          (cons 'boolean-eval (test-equal (boolean-eval '(and t (or nil t))) t))

          ;; merge-sort
          (cons 'merge-sort
                (test-equal
                 (merge-sort '(4 2 1 3) (lambda (a b) (< a b)))
                 '(1 2 3 4))))))
    ;; Print results
    (labels ((print-results (lst)
               (when lst
                 (let ((r (car lst)))
                   (format t "~A: ~A~%" (car r) (cdr r))
                   (print-results (cdr lst))))))
      (print-results results))))

(defun main ()
  (when (load "Lisp_Test.lisp")
    (run-tests)))
