#lang racket

(require racket/mpair)
(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)

(define (bstree-make)
  (mlist '() '() '()))

(define (add-elmt t k v)
  (set-mcar! t (mcons k v)))

(define (new-left t)
  (set-mcar! (mcdr t) (bstree-make)))

(define (new-right t)
  (set-mcar! (mcdr (mcdr t)) (bstree-make)))

(define (pair t)
  (mcar t))

(define (left t)
  (mcar (mcdr t )))

(define (right t)
  (mcar (mcdr (mcdr t))))

(define (key t)
  (mcar (pair t)))

(define (val t)
  (mcdr (pair t)))

(define (empty-tree? t)
  (empty? (mcar t)))

(define (pred t)
  (max-tree (left t)))

(define (succ t)
  (min-tree (right t)))

(define (max-tree t)
  (if (empty-tree? (right t)) (pair t) (max-tree (right t))))

(define (min-tree t)
  (if (empty-tree? (left t)) (pair t) (min-tree (left t))))

(define (bstree-add-elmt t k v)
  (cond
    [(empty-tree? t) (begin (add-elmt t k v) (new-left t) (new-right t) #f)]
    [(= (key t) k) (begin (add-elmt t k v) #t)]
    [(> (key t) k) (bstree-add-elmt (left t) k v)]
    [(< (key t) k) (bstree-add-elmt (right t) k v)])
  )

(define (clear t)
  (set-mcar! t '()) (set-mcar! (mcdr t) '()) (set-mcar! (mcdr (mcdr t)) '()))

(define (bstree-del-elmt t k)
  (cond
    [(empty-tree? t) #f]
    [(= (key t) k) (begin
                     (cond
                     [(and (empty-tree? (left t)) (empty-tree? (right t))) (clear t)]
                     [(not (empty-tree? (right t))) (begin
                                          (define root (succ t))
                                          (set-mcar! t root)
                                          (bstree-del-elmt (right t) (mcar root))
                                          )]
                     [(not (empty-tree? (left t))) (begin
                                           (define root (pred t))
                                           (set-mcar! t root)
                                           (bstree-del-elmt (left t) (mcar root))
                                           )])
                     #t)]
    [(> (key t) k) (bstree-del-elmt (left t) k)]
    [(< (key t) k) (bstree-del-elmt (right t) k)])
  )
 

(define (bstree-find-elmt t k)
  (cond
    [(empty-tree? t) (inr 'empty)]
    [(= (key t) k) (inl (val t))]
    [(> (key t) k) (bstree-find-elmt (left t) k)]
    [(< (key t) k) (bstree-find-elmt (right t) k)])
  )


