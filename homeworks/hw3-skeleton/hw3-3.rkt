#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

; Play around the interface of hw3-3-library.rkt.
; After executing this file, see hw3-3.ps.
; To read .ps files, you will need
;  - GhostScript: http://www.ghostscript.com/download/gsdnld.html
;  - Ghostview: http://pages.cs.wisc.edu/~ghost/gsview/

; some test code:
;(define maze1 (init-maze 4 3))
;(define maze2 (open-s 2 1 maze1))
;(maze-pp maze2)

(define (allvisited l)
  (if (null? l) #t
      (if (car l) (allvisited (cdr l))
          #f)))

(define (part lst i)
  (let-values (((head tail) (split-at lst i)))
    (list head tail)))

(define (change l n v)
  (define split (part l n))
  (append (car split) (list v) (cdadr split)))
  
  

(define (initstate n)
  (if (= 1 n) '(#f) (list* #f (initstate (- n 1)))))

(define (odd v) (if (= 0 (modulo v 2)) 0 1))
(define (even v) (if (= 0 (modulo v 2)) 1 0))

(define (cellval x y n m)
  (if (or (> 0 x) (> 0 y) (<= n y) (<= m x)) -1
  (+ x (* y m))))


(define (neighbors curr n m)
  (define x (modulo curr m))
  (define y (quotient curr m))
  (define (overflow? k) = (< k (* m n)))
  (define candidate '())
  (if (> x 0) (set! candidate (list* (cellval (- x 1) (- y (odd x)) n m) (cellval (- x 1) (+ y (even x)) n m) candidate)) 0)
  (if (< x (- m 1)) (set! candidate (list* (cellval (+ x 1) (- y (odd x)) n m) (cellval (+ x 1) (+ y (even x)) n m) candidate)) 0)
  (if (> y 0) (set! candidate (list* (cellval x (- y 1) n m) candidate)) 0)
  (if (< y (- n 1)) (set! candidate (list* (cellval x (+ y 1) n m) candidate)) 0)
  (filter overflow? (filter exact-nonnegative-integer? candidate)))
                          

(define (neighbors_unvisit nlist currstate)
  (if (null? nlist) '()
      (if (list-ref currstate (car nlist)) (neighbors_unvisit (cdr nlist) currstate) (append (list (car nlist)) (neighbors_unvisit (cdr nlist) currstate) ))))
  
 (define (choice l)
  (list-ref l (random (length l))))
  
  

(define (opencell maze currcell newcell n m)
  (define x (modulo currcell m))
  (define y (quotient currcell m))
  (define xx (modulo newcell m))
  (define yy (quotient newcell m))
  (if (= x xx)
      (if (= 1 (- yy y)) (open-s y x maze) (open-n y x maze))
      (if (= 1 (- xx x)) (if (or (< yy y) (and (= y yy) (= 0 (modulo x 2)))) (open-ne y x maze) (open-se y x maze))
          (if (or (< yy y) (and (= y yy) (= 0 (modulo x 2)))) (open-nw y x maze) (open-sw y x maze)))))
  


(define (mazeGen n m)
  (define maze (init-maze n m))
  (define stack '())
  (define currstate (initstate (* n m)))
  (define currcell (random m))
  (set! maze (open-n 0 currcell maze))
  (set! currstate (change currstate currcell #t))
  (let loop()
    (when (not (allvisited currstate))
      (define newcell -1)
      (if (not (= 0 (length (neighbors_unvisit (neighbors currcell n m) currstate))))
          (begin
            (set! newcell (choice (neighbors_unvisit (neighbors currcell n m) currstate)))
            (set! stack (list* currcell stack))
            (set! maze (opencell maze currcell newcell n m))
            (set! currcell newcell)
            (set! currstate (change currstate currcell #t)))
          (if (not (= 0 (length stack)))
              (begin
                (set! currcell (car stack))
                (set! stack (cdr stack))) 0))
      (loop)))
  (open-s (- n 1) (random m) maze)
  )
      

    