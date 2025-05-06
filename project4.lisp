(defun set-member (set item)
    (if (null set)
        nil
        (if (equal (car set) item)
            t
            (set-member (cdr set) item))))

#|(defun set-union (set-1 set-2)
  (if (null set-2)
      set-1
      (let ((item (car set-2)))
        (if (set-member set-1 item)
            (set-union set-1 (cdr set-2))
            (set-union (cons item set-1) (cdr set-2)))))) |#

(defun set-union (set-1 set-2)
  (if (null set-2)
      set-1
      (let ((item (car set-2)))
        (if (set-member set-1 item)
            (set-union set-1 (cdr set-2))
            (set-union (append set-1 (list item)) (cdr set-2))))))

(defun set-intersection (set-1 set-2)
  (if (null set-1)
      nil
      (let ((item (car set-1)))
        (if (set-member set-2 item)
            (cons item (set-intersection (cdr set-1) set-2))
            (set-intersection (cdr set-1) set-2)))))

(defun set-diff (set-1 set-2)
  (if (null set-1)
    nil
    (let((item (car set-1)))
      (if (set-member set-2 item)
        (set-diff (cdr set-1) set-2)
        (cons item (set-diff (cdr set-1) set-2))))))

(defun boolean-xor (a b)
    (if (equal a b)
    nil
    t))

(defun boolean-implies (a b)
  (or (not a) b))

(defun boolean-iff (a b)
  (if (equal a b)
    t
    nil))
  
(defun boolean-eval (exp)
  (cond
    ((atom exp) exp)
    ((equal (car exp) 'not)
     (not (boolean-eval (second exp))))
    ((equal (car exp) 'and)
     (and (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'or)
     (or (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'xor)
     (boolean-xor (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'implies)
     (boolean-implies (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'iff)
     (boolean-iff (boolean-eval (second exp)) (boolean-eval (third exp))))
    (t nil)))

(defun merge-sort (lst predicate)
  (if (or (null lst) (null (cdr lst)))  ; Base case: empty list or single element
      lst
      ;; Split the list, sort both halves, then merge
      (let* ((length (length lst))
             (middle (floor length 2))
             (left-half (subseq lst 0 middle))
             (right-half (subseq lst middle)))
        (merge-lists (merge-sort left-half predicate)
                     (merge-sort right-half predicate)
                     predicate))))

(defun merge-lists (list1 list2 predicate)
  (cond ((null list1) list2)                   ; If list1 is empty, return list2
        ((null list2) list1)                   ; If list2 is empty, return list1
        ((funcall predicate (car list1) (car list2)) ; Use predicate for comparison
         (cons (car list1) (merge-lists (cdr list1) list2 predicate)))
        (t                                     ; Otherwise
         (cons (car list2) (merge-lists list1 (cdr list2) predicate)))))
