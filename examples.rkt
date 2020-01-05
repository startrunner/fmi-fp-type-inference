#lang racket
(require "infrastructure.rkt")
(require "types.rkt")
(require "type-inference.rkt")
(require "parsing.rkt")

(define terms
 (list
  "λx.x"
  "λx.y"
  "λxy.x"
  "λfx.f(fx)"
  "λxyz.xz(yz)"
  "λxyz.λa.axyz"))

(for-each
 (lambda (term)
  {display "TERM:      "}{display term} {newline}
  {display "PARSED:    "}{print-term (parse-term term)} {newline}
  {display "INF. TYPE: "}{print-type (infer term)} {newline}
  {newline})
 terms)
