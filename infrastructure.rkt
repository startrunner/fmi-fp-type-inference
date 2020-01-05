#lang racket

(provide any?)
(provide deep-equal?)
(provide assert)
(provide list-at)
(provide ++)
(provide --)
(provide contains? )
(provide min-char)
(provide contains-key?)
  
(define (++ x) {+ x 1})
(define (-- x) {- x 1})

(define min-char
  (lambda chars
    {let*
     ([ints (map char->integer chars)]
      [minInteger (foldl min (car ints) ints)])
      (integer->char minInteger)}))

(define (assert valid? x)
  {cond
    [(not (valid? x))   (raise (string-append "INVALID ARGUMENT"))]
    [else               x]})

(define (list-at list index)
  {cond
    [(negative? index)          (raise "INVALID INDEX")]
    [(not (integer? index))     (raise "INDEX MUST BE INTEGER")]
    [(empty? list)              (raise "INDEX OUT OF BOUNDS")]
    [(zero? index)              (car list)]
    [else                       (list-at (cdr list) (-- index))]})

(define (non-list-pair? x)
  {and (pair? x) (not (list? x))}) 

(define (deep-equal? x y)
 {let*
  ([result
    {cond
    [(and (non-list-pair? x) (non-list-pair? y))
     (and (deep-equal? (car x) (car y)) (deep-equal? (cdr x) (cdr y)))]
    [(not (list? x))    (eqv? x y)]
    [(not (list? y))    (eqv? x y)]
    [(empty? x)         (empty? y)]
    [(empty? y)         (empty? x)]
    [else               (and (deep-equal? (car x) (car y)) (deep-equal? (cdr x) (cdr y)))]}])

   ;{println "COMPARISON!!!"}
   ;{println x}
   ;{println y}
   ;{println result}
   result})

;Some list operations
(define (println-list list)
  {cond
    [(empty? list)
     (println "END")]
    [else
     (println (car list))
     (println-list (cdr list))]})

(define (add-if-not-exists list item)
  {cond
    [(contains? list item)   list]
    [else                    (cons item list)]})

(define (count list predicate) {_count list predicate 0})
(define (_count list predicate result)
  {cond
    [(empty? (assert list? list))    result]
    [(predicate (car list))          (_count (cdr list) predicate (++ result))]
    [else                            (_count (cdr list) predicate result)]})

(define (any? list predicate) {> (count list predicate) 0})

(define (contains? list item) {any? list (lambda (x) {deep-equal? x item})})

(define (contains-key? map key)
  {cond
    [(empty? map)                         #f]
    [(deep-equal? (car (car map)) key)    #t]
    [else                                 (contains-key? (cdr map) key)]})



