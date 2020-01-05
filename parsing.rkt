#lang racket
(require "infrastructure.rkt")
(require "types.rkt")
(provide parse-term)

(define (first-is? list first)
  {and (list? list) (not (empty? list)) (deep-equal? (car list) first)})

(define (last-is? list last)
  {and (list? list) {first-is? (reverse list) last}})

(define (trim-start list item)
 {cond
  [(empty? list)                  `()]
  [(deep-equal? (car list) item)  (trim-start (cdr list) item)]
  [else                           list]})
(define (trim-end list item)
 {reverse (trim-start (reverse list) item)})

(define (append-list-of-lists lists)
 {_append-list-of-lists lists `()})
(define (_append-list-of-lists lists result)
 {cond
  [(empty? lists)         (reverse result)]
  [(empty? (car lists))   {_append-list-of-lists (cdr lists) result}]
  [else
   {_append-list-of-lists (cons (cdr (car lists)) (cdr lists)) (cons (car (car lists)) result)}]})

(define (upper-split text splitters)
  {cond
    [(eqv? text "")   `()]
    [(empty? text)    `()]
    [(string? text)   {_upper-split (string->list text) splitters 0 `(())}]
    [else             [_upper-split text splitters 0 `(())]]})

(define (_upper-split chars splitters depth result)
  ;{println chars}
  ;{println (car splitters)}
  ;{println depth}
  {cond
    [(negative? depth)
     {raise "INVALID BRACKETS!!!"}]
    [(empty? chars)
     (reverse (map reverse result))]
    [(and (= depth 0) (contains? splitters (car chars)))
     {_upper-split (cdr chars) splitters depth (cons `() result)}]
    [(eq? #\( (car chars))
     {_upper-split (cdr chars) splitters (++ depth) (cons (cons (car chars) (car result)) (cdr result))}]
    [(eq? #\) (car chars))
     {_upper-split (cdr chars) splitters (-- depth) (cons (cons (car chars) (car result)) (cdr result))}]
    [else
     {_upper-split (cdr chars) splitters depth (cons (cons (car chars) (car result)) (cdr result))}]})

(define (verify-brackets chars)
  {cond
    [(eqv?
      "" chars)   #t]
    [(empty? chars)    #t]
    [(string? chars)   {_verify-brackets (string->list chars) 0}]
    [else              [_verify-brackets chars 0]]})
(define (_verify-brackets chars depth)
  {cond
    [(negative? depth)         #f]
    [(empty? chars)            (zero? depth)]
    [(eq? #\( (car chars))     {_verify-brackets (cdr chars) (++ depth)}]
    [(eq? #\) (car chars))     {_verify-brackets (cdr chars) (-- depth)}]
    [else                      {_verify-brackets (cdr chars) depth}]})

(define (bracket-wrapped? chars)
  {let*
   ([charList {if (string? chars) (string->list chars) chars}]
    [len (length charList)])
    {cond
     [(< len 2)                                 #f]
     [(not (eqv? #\( (car charList)))           #f]
     [else {and (verify-brackets charList)      (verify-brackets (take (cdr charList) (- len 2)))}]}})

(define lambdaChars (list #\\ #\λ))

(define (tokenize chars)
 {cond
  [(string? chars)    (tokenize (string->list chars))]
  [else               {_tokenize chars 0 `()}]})
(define (_tokenize chars level result)
 {cond
  [(empty? chars)         (reverse (map reverse result))]
  [(and (zero? level) (not (eqv? (car chars) #\()) (not (eqv? (car chars) #\))))
   {_tokenize (cdr chars) level (cons (list (car chars)) result)}]
  [(and (eqv? #\( (car chars)) (zero? level))
   {_tokenize (cdr chars) (++ level) (cons (list #\() result)}]
  [(eqv? #\( (car chars))
   {_tokenize (cdr chars) (++ level) (cons (cons (car chars) (car result)) (cdr result))}]
  [(eqv? #\) (car chars))
   {_tokenize (cdr chars) (-- level) (cons (cons #\) (car result)) (cdr result))}]
  [else
   {_tokenize (cdr chars) level (cons (cons (car chars) (car result)) (cdr result))}]})

(define (build-application terms)
 {_build-application (cdr terms) (car terms)})
(define (_build-application terms result)
 {cond
  [(empty? terms)        result]
  [else                  {_build-application (cdr terms) (make-application result (car terms))}]})

(define (parse-application chars)
 {cond
  [(empty? chars)       (raise "INVALID EXPRESSION! APPLICATION EXPECTED!")]
  [(= 1 (length chars)) (car chars)]
  [else
   {let*
    ([tokens (tokenize chars)]
     [terms (map parse-term tokens)])
     ;{display "parsing application"} {newline}
     ;{display chars} {newline}
     {build-application terms}}]})

(define (build-nested-lambda args innerExpression)
 {_build-nested-lambda (reverse args) innerExpression})
(define (_build-nested-lambda revArgs result)
  {cond
   [(empty? revArgs)
    result]
   [(not (char? (car revArgs)))
    (raise "INVALID LAMBDA ARGUMENT! VARIABLE EXPECTED!!!")]
   [else
    {_build-nested-lambda (cdr revArgs) (make-lambda (car revArgs) result)}]})

(define (parse-singleton-lambda chars)
 {let*
  ([dotSplit (upper-split chars (list #\.))]
   [dotSplitLen (length dotSplit)])
   {cond
    [(not (eq? dotSplitLen 2))
     (raise "IVALID LAMBDA! A SINGLE DOT EXPECTED!")]
    [else {build-nested-lambda (car dotSplit) (parse-term (car (cdr dotSplit)))}]}})

(define (parse-lambda lambdaSplit)
 {let*
  ([splitLen (length lambdaSplit)])
   {cond
    [(< splitLen 2)
     (raise "CAN'T PARSE THIS WEIRD EMPTY LAMBDA!")]
    [(not (empty? (car lambdaSplit)))
     {make-application (parse-application (car lambdaSplit)) (parse-lambda (cons `() (cdr lambdaSplit)))}]
    [(= splitLen 2)
     {parse-singleton-lambda (car(cdr lambdaSplit))}]
    [else
     {parse-singleton-lambda
      (append-list-of-lists (map (lambda (x) {trim-end x #\.}) (cdr lambdaSplit)))}]}})

(define (parse-term text)
 {let*
  ([chars (assert list? {if (string? text) (string->list text) text})]
   [len (length chars)])
   {cond
    [(zero? len)                        (raise "CAN'T PARSE EMPTY EXPRESSION!")]
    [(not (verify-brackets chars))      (raise "CAN'T PARSE EXPRESSION WITH INVALID BRACKETS!")]
    [(bracket-wrapped? chars)           {parse-term (take (cdr chars) (- len 2))}]
    [else
     {let*
      ([lambdaSplit (upper-split chars lambdaChars)]
       [lambdaSplitLen (length lambdaSplit)])
      {cond
       [(= lambdaSplitLen 1) {parse-application (car lambdaSplit)}]
       [else                 {parse-lambda lambdaSplit}]}}]}})
