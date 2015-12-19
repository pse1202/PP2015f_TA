#lang racket

(provide myeval)

; Read the instruction of document carefully,
; and then write code according to it.

; This `myeval` function should mimic the behavior of `eval`,
; which is a built-in function of Racket.

; You should raise exceptions for irregular situations.
; The exception message doesn't have to be the same as `eval`,
; but the message should contain useful information.

(define (myeval e)
  (eval-env e (emptyenv)))

(define (emptyenv) identity)

(define (bind def env)
  (lambda (x) (if (eq? x (car def)) (eval-env (car (cdr def)) env) (search env (car def)))))

(define (search env var)
  (if (eq? (env var) var) (raise (error "Unbound Variable")) (env var)))

(define (base? e)
  (or (boolean? e)
      (number? e)
      (char? e)
      (string? e)
      (bytes? e)
      ))
(define (def? def)
  (and (= 2 (length def)) (symbol? (car def))))

(define (valid-def? defs)
  (andmap def? defs))

(define (eval-env e env)
  (if (base? e) e
      (if (equal? e ''()) '()
          (if (symbol? e) (search env e)
          (if (list? e)
          (match e
            [(list '< e1 e2) (< (eval-env e1 env) (eval-env e2 env))]
            [(list '> e1 e2) (> (eval-env e1 env) (eval-env e2 env))]
            [(list '= e1 e2) (= (eval-env e1 env) (eval-env e2 env))]
            [(list '+ e1 e2) (+ (eval-env e1 env) (eval-env e2 env))]
            [(list '- e1 e2) (- (eval-env e1 env) (eval-env e2 env))]
            [(list '* e1 e2) (* (eval-env e1 env) (eval-env e2 env))]
            [(list 'cons e1 e2) (cons (eval-env e1 env) (eval-env e2 env))]
            [(list 'car e1) (car (eval-env e1 env))]
            [(list 'cdr e1) (cdr (eval-env e1 env))]
            [(list 'if e1 e2 e3) (if (eval-env e1 env) (eval-env e2 env) (eval-env e3 env))]
            [(list 'let defs body) (if (valid-def? defs)
                                         (eval-env body (foldl bind env defs))
                                         (raise (error "let : bad syntax")))]
            [(list 'letrec defs body) 'TODO]
            [(list 'lambda args body) 'TODO]
            [else 'TODO]
            )
          (raise (error "Syntax Error")))))))