;;;; Load your function definitions
(load "Lisp_Test.lisp")

;;;; Define a test case structure
(defstruct test-case
  name
  form
  expected)

;;;; Run a single test case
(defun run-test-case (tc)
  (let* ((result (eval (test-case-form tc)))
         (expected (test-case-expected tc)))
    (if (equal result expected)
        (format t "~A: PASS~%" (test-case-name tc))
        (format t "~A: FAIL~%  Expected: ~S~%  Got:      ~S~%~%" 
                (test-case-name tc) expected result))))

;;;; Run a list of test cases
(defun run-all-tests (tests)
  (format t "Running ~D tests...~%~%" (length tests))
  (mapcar #'run-test-case tests))

;;;; Define your test cases here
(defparameter my-tests
  (list
   ;; set-member
   (make-test-case :name "set-member 1 in (1 2)" :form '(set-member '(1 2) 1) :expected t)
   (make-test-case :name "set-member 3 in (1 2)" :form '(set-member '(1 2) 3) :expected nil)

   ;; set-union
   (make-test-case :name "set-union (1 2) (2 4)" :form '(set-union '(1 2) '(2 4)) :expected '(1 2 4))

   ;; set-intersection
   (make-test-case :name "set-intersection (1 2) (2 4)" :form '(set-intersection '(1 2) '(2 4)) :expected '(2))

   ;; set-diff
   (make-test-case :name "set-diff (1 2) (2 4)" :form '(set-diff '(1 2) '(2 4)) :expected '(1))

   ;; boolean logic
   (make-test-case :name "boolean-xor t nil" :form '(boolean-xor t nil) :expected t)
   (make-test-case :name "boolean-xor nil nil" :form '(boolean-xor nil nil) :expected nil)
   (make-test-case :name "boolean-implies t nil" :form '(boolean-implies t nil) :expected nil)
   (make-test-case :name "boolean-implies nil nil" :form '(boolean-implies nil nil) :expected t)
   (make-test-case :name "boolean-iff t t" :form '(boolean-iff t t) :expected t)
   (make-test-case :name "boolean-iff t nil" :form '(boolean-iff t nil) :expected nil)

   ;; boolean-eval
   (make-test-case :name "boolean-eval (and t nil)" :form '(boolean-eval '(and t nil)) :expected nil)
   (make-test-case :name "boolean-eval (or nil t)" :form '(boolean-eval '(or nil t)) :expected t)
   (make-test-case :name "boolean-eval (xor t nil)" :form '(boolean-eval '(xor t nil)) :expected t)
   (make-test-case :name "boolean-eval (implies t nil)" :form '(boolean-eval '(implies t nil)) :expected nil)
   (make-test-case :name "boolean-eval (iff nil nil)" :form '(boolean-eval '(iff nil nil)) :expected t)

   ;; merge-sort
   (make-test-case :name "merge-sort ascending" :form '(merge-sort '(2 1 5 0) #'<) :expected '(0 1 2 5))
   (make-test-case :name "merge-sort descending" :form '(merge-sort '(2 1 5 0) #'>) :expected '(5 2 1 0))))

;;;; Run all tests
(run-all-tests my-tests)