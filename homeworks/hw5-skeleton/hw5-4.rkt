#lang racket

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)


;;; primitive tile

(define black ; black: form
  'B)
(define white ; white: form
  'W)


;;; complex tile
;
; An array tile looks like:
; (cons 'array (list row_1 row_2 ... row_n)),
; for each row_i = (list cell_i1 ... cell_in).
;
; Examples:
; 1.
; (cons 'array (list (list 'B 'B) (list 'W 'W)))
; BB
; WW
;
; 2.
; (cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B)))
; BBBB
; BBBB
; WWBB
; WWBB
;
;
; An tree tile looks like:
; (cons 'tree (list subtree_nw subtree_ne subtree_se subtree_sw)).
;
; Examples:
; 1.
; (cons 'tree (list 'B 'B 'W 'B))
; BB
; BW
;
; 2.
; (cons 'tree (list (list 'B 'B 'B 'W) (list 'B 'B 'W 'B) (list 'B 'W 'B 'B) (list 'W 'B 'B 'B)))
; BBBB
; WBBW
; WBBW
; BBBB
;
; See hw5-4-selfgrader.rkt for more details on grading array and tree representation.

(define (basis? f) (and (is-tree? f) (is-array? f)))

(define (glue-array-from-tree nw ne se sw) ; glue-array-from-tree: form * form * form * form -> form
  (glue-array-from-array (tree-to-array nw) (tree-to-array ne) (tree-to-array se) (tree-to-array sw)))

(define (glue-array-from-array nw ne se sw) ; glue-array-from-array: form * form * form * form -> form
  (if (basis? nw) (cons 'array (list (list nw ne) (list sw se))) (cons 'array (mergeh (mergev (cdr nw) (cdr ne)) (mergev (cdr sw) (cdr se))))))

(define (glue-tree-from-tree nw ne se sw) ; glue-tree-from-tree: form * form * form * form -> form
  (if (basis? nw) (cons 'tree (list nw ne se sw)) (cons 'tree (list (cdr nw) (cdr ne) (cdr se) (cdr sw)))))

(define (glue-tree-from-array nw ne se sw) ; glue-tree-from-array: form * form * form * form -> form
  (glue-tree-from-tree (array-to-tree nw) (array-to-tree ne) (array-to-tree se) (array-to-tree sw)))

(define (rotate-array f) ; rotate-array: form -> form
  (tree-to-array (rotate-tree (array-to-tree f))))

(define (neighbor-array location f) ; neighbor-array: location * form -> int
  (+ (latticeblack? (cdr f) (loc-to-lattice location (cons 1 1)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons 0 1)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons -1 1)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons 1 0)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons -1 0)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons 1 -1)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons 0 -1)))
     (latticeblack? (cdr f) (loc-to-lattice location (cons -1 -1)))))

(define (latticeblack? f l)
  (if (or (< (car l) 0) (< (cdr l) 0) (>= (cdr l) (length f)) (>= (car l) (length f))) 0
  (if (eq? black (nthelem (nthelem f (cdr l)) (car l))) 1 0)))
  
 
(define (loc-to-lattice loc curr)
  (if (null? loc) curr
      (loc-to-lattice (cdr loc) (newc curr (expt 2 (- (length loc) 1)) (car loc)))))

(define (nthelem l n)
  (if (= 0 n) (car l)
      (nthelem (cdr l) (- n 1))))

(define (newc curr l x)
      (cons (+ (car curr) (locl l x)) (+ (cdr curr) (locr l x))))
(define (locl l x)
  (if (or (= x 1) (= x 2)) l 0))
(define (locr l x)
  (if (or (= x 2) (= x 3)) l 0))

(define (halflen l) (/ (length l) 2))
(define (lefthalf l) (take l (halflen l)))
(define (righthalf l) (take-right l (halflen l)))
(define (array-nw a) (if (= 2 (length a)) (car(car a)) (map lefthalf (lefthalf a))))
(define (array-ne a) (if (= 2 (length a)) (cadr(car a)) (map righthalf (lefthalf a))))
(define (array-sw a) (if (= 2 (length a)) (car(cadr a)) (map lefthalf (righthalf a))))
(define (array-se a) (if (= 2 (length a)) (cadr(cadr a)) (map righthalf (righthalf a))))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
; Read hw5-4-selfgrader.rkt for formatting.
(define (pprint-array f) ; pprint-array: form -> string
  (arrayreduce (cdr f) ""))

(define (arrayreduce l s)
  (string-append* (map list-to-string l)))

(define (list-to-string f)
  (list->string (append (map (lambda (x) (if (eq? x black) #\B #\W)) f) (list #\newline))))

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'array (car f)) #t]
        [else #f]))


;;; implementation with tree

(define (raw-to-tree t) (if (basis? t) t (cons 'tree t)))

(define (rotate-tree t) ; rotate-tree: form -> form
  (if (basis? t) t
  (cons 'tree (rotate-rawtree (cdr t)))))
(define (rotate-rawtree t)
  (if (basis? t) t
      (list (rotate-rawtree (rawtree-sw t)) (rotate-rawtree (rawtree-nw t)) (rotate-rawtree (rawtree-ne t)) (rotate-rawtree (rawtree-se t)))))

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  (neighbor-array loc (tree-to-array f)))

(define (tree-nw f) (cadr f))
(define (tree-ne f) (caddr f))
(define (tree-se f) (cadddr f))
(define (tree-sw f) (car (cddddr f)))
(define (rawtree-nw t) (car t))
(define (rawtree-ne t) (cadr t))
(define (rawtree-se t) (caddr t))
(define (rawtree-sw t) (cadddr t))


; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint-tree f) ; pprint-tree: form -> string
  (pprint-array (tree-to-array f)))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'tree (car f)) #t]
        [else #f]))


;;; conversions 


(define (array-to-tree f) ; array-to-tree: form -> form
  (if (is-tree? f) f (cons 'tree (rawarray-to-rawtree (cdr f)))))
      
(define (rawarray-to-rawtree f)
  (if (basis? f) f
  (list (rawarray-to-rawtree (array-nw f)) (rawarray-to-rawtree (array-ne f)) (rawarray-to-rawtree (array-se f)) (rawarray-to-rawtree (array-sw f)))))

(define (tree-to-array f) ; tree-to-array: form -> form
  (if (is-array? f) f (cons 'array (rawtree-to-rawarray (cdr f)))))

(define (rawtree-to-rawarray f)
  (if (basis? f) f
  (mergeh (mergev (rawtree-to-rawarray (rawtree-nw f)) (rawtree-to-rawarray (rawtree-ne f))) (mergev (rawtree-to-rawarray (rawtree-sw f)) (rawtree-to-rawarray (rawtree-se f))))))

(define (mergev l1 l2)
  (if (list? l1) (map append l1 l2)
  (list l1 l2)))

(define (mergeh l1 l2)
  (if (basis? (car l1)) (list l1 l2)
  (append l1 l2)))

;;; interfaces

(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  (glue-tree-from-tree (array-to-tree nw) (array-to-tree ne) (array-to-tree se) (array-to-tree sw)))

(define (rotate f) ; rotate: form -> form
  (if (is-array? f)
      (rotate-array f)
      (rotate-tree f)))

(define (neighbor loc f) ; neighbor: location * form -> int
  (if (is-array? f)
      (neighbor-array loc f)
      (neighbor-tree loc f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint f) ; pprint: form -> string
  (if (is-array? f)
      (pprint-array f)
      (pprint-tree f)))
