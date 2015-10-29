#lang racket


; We auto-grade only "react" function; other functions are not
; auto-graded. However, S, K, I, v, and a are required for
; grading. See hw4-2-grade.rkt for more information.
(provide react S K I v a)
(require racket/match)

; Implement react. 
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".

(define (react e) ; execute: solution -> string
  (pprint 
   (process e)
   ))

(define (proc e)
  (match e
    [(cons 'I x) x]
    [(cons (cons 'K x) y) x]
    [(cons (cons (cons 'S x) y) z) (cons (cons x z) (cons y z))]
    [(cons x y) (cons (proc x) (proc y))]
    [_ e]))
(define (process e)
  (if (equal? e (proc e)) e
      (process (proc e))))
  

(define S ; S: solution
  'S)
(define K ; K: solution
  'K)
(define I ; I: solution
  'I)
(define (v str) ; v: string -> solution
  str)
(define (a lhs rhs) ; a: solution * solution -> solution
  (cons lhs rhs))


; You may need the following tree interface.

(define (isS? e) ; isS?: solution -> bool
  (equal? 'S e))
(define (isK? e) ; isK?: solution -> bool
  (equal? 'K e))
(define (isI? e) ; isI?: solution -> bool
  (equal? 'I e))
(define (isv? e) ; isv?: solution -> bool
  (string? e))
(define (isa? e) ; isa?: solution -> bool
  (pair? e))
(define (var e) ; var: solution -> string
  e)
(define (al e) ; al: solution -> solution
  (car e))
(define (ar e) ; ar: solution -> solution
  (cdr e))
(define (pprint e) ; pprint: solution -> string
  (match e
    ['S "S"]
    ['K "K"]
    ['I "I"]
    [(cons x y) (string-append "(" (pprint x) " " (pprint y) ")")]
    [_ e]))
