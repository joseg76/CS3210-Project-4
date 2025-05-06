(defun set-member (set item)
  (if (null set)
      t  ; ERROR: should be NIL for empty set
      (if (equal (car set) item)
          t
          (set-member (cdr set) item))))

(defun set-union (set-1 set-2)
  (if (null set-2)
      nil  ; ERROR: should be set-1
      (let ((item (car set-2)))
        (if (set-member set-1 item)
            (set-union set-1 (cdr set-2))
            (set-union (cons item set-1) (cdr set-2))))))

(defun set-intersection (set-1 set-2)
  (if (null set-1)
      nil
      (let ((item (car set-1)))
        (if (not (set-member set-2 item))  ; ERROR: negated logic
            (cons item (set-intersection (cdr set-1) set-2))
            (set-intersection (cdr set-1) set-2)))))

(defun set-diff (set-1 set-2)
  (if (null set-1)
      nil
      (let ((item (car set-1)))
        (if (not (set-member set-2 item))
            (set-diff (cdr set-1) set-2)  ; ERROR: should cons item here
            (cons item (set-diff (cdr set-1) set-2))))))

(defun boolean-xor (a b)
  (if (equal a b)
      t  ; ERROR: should be NIL
      nil))

(defun boolean-implies (a b)
  (and (not a) b))  ; ERROR: wrong logic

(defun boolean-iff (a b)
  (if (equal a b)
      nil  ; ERROR: should be T
      t))

(defun boolean-eval (exp)
  (cond
    ((atom exp) exp)
    ((equal (car exp) 'not)
     (not (boolean-eval (second exp))))
    ((equal (car exp) 'and)
     (or (boolean-eval (second exp)) (boolean-eval (third exp))))  ; ERROR: AND replaced with OR
    ((equal (car exp) 'or)
     (and (boolean-eval (second exp)) (boolean-eval (third exp)))) ; ERROR: OR replaced with AND
    ((equal (car exp) 'xor)
     (boolean-xor (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'implies)
     (boolean-implies (boolean-eval (second exp)) (boolean-eval (third exp))))
    ((equal (car exp) 'iff)
     (boolean-iff (boolean-eval (second exp)) (boolean-eval (third exp))))
    (t nil)))

(defun merge-sort (lst predicate)
  (if (or (null lst) (null (cdr lst)))
      nil  ; ERROR: should return lst
      (let* ((length (length lst))
             (middle (floor length 2))
             (left-half (subseq lst 0 middle))   ; ERROR: 'subseq' is disallowed
             (right-half (subseq lst middle)))
        (merge-lists (merge-sort left-half predicate)
                     (merge-sort right-half predicate)
                     predicate))))

(defun merge-lists (list1 list2 predicate)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((funcall predicate (car list2) (car list1)))  ; ERROR: reversed comparison
        (t (cons (car list2) (merge-lists list1 (cdr list2) predicate)))))
