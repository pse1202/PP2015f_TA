#lang racket


; We auto-grade only vlencode function; other functions are not
; auto-graded.
; If this "provide" statement is omitted, your submission will be graded 0.
(provide vlencode)


(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  (if (= 0 (length frequencies)) '()
      (if (= 1 (length frequencies)) (list (cons (car (car frequencies)) (list 0)))
  (listify (encode (treefy frequencies)) '())
  )))

; recursive
; (tree * int list) -> (tree * int list)
; order of tree : number of leaf nodes
; tree is reduced to subtrees in recursion -> finally becomes a leaf node
(define (listify tree currlist)
  (if (isleaf? tree) (list (cons (cdr tree) (reverse currlist)))
      (append (listify (leftsub tree) (list* 0 currlist)) (listify (rightsub tree) (list* 1 currlist)))))

(define (treefy freq)
  (filter (lambda (x) (> (leafval x) 0)) (map (lambda (x) (leaf (car x) (cdr x))) freq)))

; recursive
; (tree list -> tree)
; order of tree list : length
; lngth is reduced by 1 until it reaches 1 (by car treel)
(define (encode treel)
  (if (<= (length treel) 1) (car treel) (encode (reduce treel))))

(define (reduce treelist)
  (define sortlist (sort treelist < #:key treeval))
  (define l (car sortlist))
  (define r (cadr sortlist))
  (if (<= (length sortlist) 1) treelist
      (list* (node l (+ (treeval l) (treeval r)) r) (cddr sortlist))))
      

;  The output of vlencode should follow the following form.
;  The exact code for each word can be different from this example,
;   but the length of the code for each word should be the same.
;  
;   (define frequencies (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0)) (cons "b" (list 1 0)) (cons "c" (list 1 1 0)) (cons "d" (list 1 1 1)))
;
;   (define frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))
;
;   (define frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6) (cons "e" 0)))
;   (vlencode frequencies) =
;     (list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))
;



; You may need the following tree interface (but not mandatory.)

(define (leaf str val) ; leaf: string * value -> tree
  (cons val str))

(define (node lsub val rsub) ; node: tree * value * tree -> tree
  (cons val (cons lsub rsub)))

(define (isleaf? tree) ; isleaf?: tree -> bool
  (string? (cdr tree)))

(define (leftsub tree) ; leftsub: tree -> tree
  (car (cdr tree)))

(define (rightsub tree) ; rightsub: tree -> tree
  (cdr (cdr tree)))

(define (leafval tree) ; leafval: tree -> value
  (car tree))

(define (leafstr tree) ; leftstr: tree -> string
  (cdr tree))

(define (rootval tree) ; rootval: tree -> value
  (car tree))

(define (treeval tree)
  (car tree))

