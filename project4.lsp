(defun set-member (set item)

  ;;Your implementation go here
    (COND
        ((null set) nil)
        ((eq item (car set)) t)
        ((set-member (cdr set) item))
    )
)


(defun set-union (set-1 set-2)

  ;;Your implementation go here

)



(defun set-intersection (set-1 set-2)

  ;;Your implementation go here

)


(defun set-diff (set-1 set-2)

  ;;Your implementation go here

)

(defun boolean-xor (a b)

  ;;Your implementation go here

)

(defun boolean-implies (a b)

;;<Your implementation go here >

)

(defun boolean-iff (a b)

;;<Your implementation go here >

)

(defun boolean-eval (exp)

;;<Your implementation go here >

)

(defun merge-sort (list predicate)

;;<Your implementation go here >

)