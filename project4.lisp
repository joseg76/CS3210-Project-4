(defun set-member (set item)
    (if (null set)
        nil
        (if (equal (car set) item)
            t
            (set-member (cdr set) item))))

(defun set-union (set-1 set-2)
  (if (null set-2)
      set-1
      (let ((item (car set-2)))
        (if (set-member set-1 item)
            (set-union set-1 (cdr set-2))
            (set-union (cons item set-1) (cdr set-2))))))

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