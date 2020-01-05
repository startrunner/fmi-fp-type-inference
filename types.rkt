#lang racket
(require "infrastructure.rkt")

(provide term?)
(provide lambda?)
(provide application?)
(provide variable?)
(provide make-application)
(provide make-lambda)
(provide application-function)
(provide application-arg)
(provide lambda-arg)
(provide lambda-body)
(provide print-type)
(provide print-term)
(provide isomorphic-types?)
(provide type?)
(provide function-type?)
(provide function-result-type)
(provide function-arg-type)
(provide primitive-type-usage)
(provide contains-primitive?)

(define _LAMBDA #\λ)
(define _APPLICATION #\A)

(define (term? x)
  {cond
    [char? x                    #t]
    [(not (list? x))            #f]
    [(eq? _LAMBDA (car x))        #t]
    [(eq? _APPLICATION (car x))   #t]
    [else                       #f]})

(define (lambda? x)         {and (list? x) (term? x) (eq? _LAMBDA (car x))})
(define (application? x)    {and (list? x) (term? x) (eq? _APPLICATION (car x))})
(define (variable? x)       {char? x})

;(define (make-variable x)                    {list _VARIABLE (assert char? x)})
(define (make-lambda arg body)               {list _LAMBDA (assert char? arg) (assert term? body)})
(define (make-application function arg)      {list _APPLICATION (assert term? function) (assert term? arg)})

(define (lambda-arg l) {list-at (assert lambda? l) 1})
(define (lambda-body l)     {list-at (assert lambda? l) 2})

(define (application-function a) {list-at (assert application? a) 1})
(define (application-arg a)      {list-at (assert application? a) 2})

(define (print-type type)
  {cond
    [(char? type)
     (display type)]
    [else
     {display #\(}
     (print-type (car type))
     {display #\→}
     (print-type (cdr type))
     {display #\)}]})

(define (print-term term)
  {cond
    [(char? term)
     (display term)]
    [(application? term)
     (display #\()
     (print-term (application-function term))
     (print-term (application-arg term))
     (display #\))]
    [(lambda? term)
     (display #\λ)
     (display (lambda-arg term))
     (display #\.)
     (print-term (lambda-body term))]})

(define (isomorphic-types? x y)
  {cond
    [(or (not (type? x)) (not (type? y)))  #f]
    [(char? x)                             (char? y)]
    [(char? y)                             (char? x)]
    [(and (pair? x) (pair? y))
     (and (isomorphic-types? (car x) (car y)) (isomorphic-types? (cdr x) (cdr y)))]
    [else #f]})

(define (type? x)
  {cond
    [(char? x)                                          #t]
    [(and (pair? x) (type? (car x)) (type? (cdr x)))    #t]
    [else                                               #f]})

(define primitive-type? char?)

(define (function-type? x)
  {and (type? x) (pair? x)})

(define (function-arg-type x)
  {cond
    [(not (function-type? x))     (raise "CAN'T GET ARG TYPE OF NON-FUNCTION TYPE")]
    [else                       (car x)]})

(define (function-result-type x)
  {cond
    [(not (function-type? x))     (raise "CAN'T GET RESULT TYPE OF NON-FUNCTION TYPE")]
    [else                       (cdr x)]})

(define (primitive-type-usage x)
  {cond
    [(not (type? x))
     (raise "CAN'T GET PRIMITIVE TYPE USAGE OF NON-TYPE VARIABLE")]
    [(char? x)
     1]
    [(function-type? x)
     (+ (primitive-type-usage (function-arg-type x)) (primitive-type-usage (function-result-type x)))]
    [else
     (raise "WTF!???")]})

(define (contains-primitive? type primitive)
 {cond
  [(not (char? primitive)) #f]
  [(not (type? type))      #f]
  [(char? type)            {eqv? type primitive}]
  [(function-type? type)
   {or (contains-primitive? (function-arg-type type) primitive) (contains-primitive? (function-result-type type) primitive)}]
  [else (raise "DIDN'T HANDLE CHECK FOR THIS TYPE :/")]})