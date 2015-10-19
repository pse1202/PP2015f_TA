#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (maze-check maze start end)
  (define oldset empty-set)
  (define newset (add-element start empty-set))
  (define oldlist null)
  (define newlist (can-enter start maze))
  (let loop()
    (set! oldset newset)
    (set! newset (addlist newlist oldset))
    (set! oldlist newlist)
    (set! newlist (remove-duplicates (can-enter-list oldlist maze) same-room? ))
    (when (not (is-sameset? oldset newset)) (loop)))
  (if (is-member? end newset) #t #f)
  )

(define (addlist roomlist roomset)
  (if (null? roomlist) roomset
      (if (is-member? (car roomlist) roomset)
      (addlist (cdr roomlist)  roomset)
      (addlist (cdr roomlist) (add-element (car roomlist) roomset))
      )))

(define (is-sameset? seta setb)
  (and (is-subset? seta setb) (is-subset? setb seta)))


(define (can-enter-list roomlist maze)
  (if (null? roomlist) null
      (append (can-enter (car roomlist) maze) (can-enter-list (cdr roomlist) maze))))
  