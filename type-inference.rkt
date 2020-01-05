#lang racket

(require "infrastructure.rkt")
(require "parsing.rkt")
(require "types.rkt")

(provide infer)

(define (infer term)
 {cond
  [(string? term)     {_infer (parse-term term)}]
  [(term? term        {_infer term})]
  [else               {raise "CANNOT INFER TYPE OF INVALID TERM!!!"}]})

(define (_infer term)
  {let*
   ([_current-greek #\α]
    [next-greek
     (lambda
      ()
      (set! _current-greek (integer->char (++ (char->integer _current-greek))))
      (integer->char (-- (char->integer _current-greek))))]
    [proposals `()]
    [print-proposal
     (lambda (proposal)
       {print-term (car proposal)}
       {display ": "}
       {print-type (cdr proposal)}
       {newline})]
    [print-proposals
     (lambda () {for-each print-proposal proposals})]
    [propose (lambda (term type) {set! proposals (cons (cons term type) proposals)})]
    [_make-initial-proposals
     (lambda
      (self x)
       {cond
         [(variable? x)
          (if (not (contains-key? proposals x)) (propose x (next-greek)) 0)]
         [(lambda? x)
          (self self (lambda-arg x))
          (self self (lambda-body x))
          (if (not (contains-key? proposals x)) (propose x (next-greek)) 0)]
         [(application? x)
          (self self (application-function x))
          (self self (application-arg x))
          (if (not (contains-key? proposals x)) (propose x (next-greek)) 0)]
         })]
    [make-initial-proposals
     (lambda (term) {_make-initial-proposals _make-initial-proposals term})]
    [foreach-proposal
     (lambda (term fn)
       {for-each
        (lambda (y) (fn (cdr y)))
        (filter (lambda (x) {deep-equal? (car x) term}) proposals)})]
    [_propose-for-term
     (lambda (self term)
       {cond
         [(lambda? term)
          {self self (lambda-body term)}
          {foreach-proposal
           (lambda-body term)
           (lambda (bodyProposal)
             {foreach-proposal
              (lambda-arg term)
              (lambda (argProposal)
                {propose term (cons argProposal bodyProposal)})})}]
         [(application? term)
          {let*
           ([resultType (next-greek)]
            [appFunction (application-function term)]
            [appArg (application-arg term)])
            
           {self self appFunction}
           {self self appArg}
           {foreach-proposal
            appArg
            (lambda
             (argProposal)
              {propose appFunction (cons argProposal resultType)})}}]
         [(variable? term)
          "Nothing more we can do after initial proposition..."]})]
    [propose-for-term
     (lambda (term) {_propose-for-term _propose-for-term term})]

    [sameTypePairs `()]
    [print-same-type-pair
     (lambda (types)
         {print-type (car types)}
         {display " <-> "}
         {print-type (cdr types)}
         {newline})]
    [print-same-type-pairs
     (lambda ()
      {for-each print-same-type-pair sameTypePairs})]
    [add-same-type-pair
     (lambda (x y)
       {cond
         [(and (not (deep-equal? x y)) (not (contains? sameTypePairs (cons x y))) (not (contains? sameTypePairs (cons y x))))
          {set! sameTypePairs (cons (cons x y) sameTypePairs)}]})]
    [initialize-same-type-pairs
     (lambda ()
      {for-each
       (lambda (proposal1)
        {for-each
         (lambda (proposal2)
           {cond
             [(deep-equal? proposal1 proposal2) "nothing to do"]
             [else
              {let*
               ([term1 (car proposal1)]
                [term2 (car proposal2)]
                [type1 (cdr proposal1)]
                [type2 (cdr proposal2)])
                {cond
                  [(deep-equal? term1 term2)
                   {add-same-type-pair type1 type2}]
                  [(and 
                    (application? term1)
                    (application? term2)
                    (function-type? type1)
                    (function-type? type2)
                    (deep-equal? (application-function term1) (application-function term2)))
                   {add-same-type-pair (function-arg-type type1) (function-arg-type type2)}
                   {add-same-type-pair (function-result-type type1) (function-result-type type2)}]
                  [(and
                    (application? term1)
                    (function-type? type2)
                    (deep-equal? (application-function term1) term2))
                   {add-same-type-pair (function-result-type type2) type1}]
                  [else "???"]}}]})
         proposals})
       proposals})]
    [_add-same-type-pairs-from-isomorphic-types
     (lambda (self type1 type2)
       {cond
         [(not (isomorphic-types? type1 type2)) "nothing to do here..."]
         [(deep-equal? type1 type2)             "nothing to do here..."]
         [(nor (contains? sameTypePairs (cons type1 type2)) (contains? sameTypePairs (cons type2 type1)))
          {set! sameTypePairs (cons (cons type1 type2) sameTypePairs)}
          {self self type1 type2}]
         [(function-type? type1)
          {self self (function-arg-type type1) (function-arg-type type2)}
          {self self (function-result-type type1) (function-result-type type2)}]})]
    [add-same-type-pairs-from-isomorphic-types
     (lambda (type1 type2) {_add-same-type-pairs-from-isomorphic-types _add-same-type-pairs-from-isomorphic-types type1 type2})]
    [_advance-same-type-pairs
     (lambda (self)
       {let*
        ([oldLength (length sameTypePairs)]
         [newLength 0])

         {for-each
          (lambda (pair)
           {cond
            [(deep-equal? (car pair) (cdr pair))
             "nothing to do here..."]
            [(isomorphic-types? (car pair) (cdr pair))
             {add-same-type-pairs-from-isomorphic-types (car pair) (cdr pair)}]
            [else "nothing to do"]})
          sameTypePairs}

         {for-each
          (lambda (pair1)
            {for-each
             (lambda (pair2)
               {let
                ([a (car pair1)] [b (cdr pair1)] [c (car pair2)] [d (cdr pair2)])
                {cond
                 [(deep-equal? pair1 pair2)
                  "nothing to do"]
                 [(or (deep-equal? a c) (deep-equal? a d) (deep-equal? b c) (deep-equal? b d))
                  {add-same-type-pair a b}
                  {add-same-type-pair a c}
                  {add-same-type-pair b c}
                  {add-same-type-pair b d}
                  {add-same-type-pair c d}
                  {add-same-type-pair a d}]}})
             sameTypePairs})
          sameTypePairs}
         
         {set! newLength (length sameTypePairs)}
         {if (< oldLength newLength) (self self) "done"}
         })]
    [advance-same-type-pairs
     (lambda () {_advance-same-type-pairs _advance-same-type-pairs})]
    [longest-primitive-equivalent
     (lambda (letter)
       {let*
        ([equalPairs1 (filter (lambda (x) (eqv? (car x) letter)) sameTypePairs)]
         [equalPairs2 (filter (lambda (x) (eqv? (cdr x) letter)) sameTypePairs)]
         [equals1 (map cdr equalPairs1)]
         [equals2 (map car equalPairs2)]
         [equals (append equals1 equals2 (list letter))]
         [withLengths (map (lambda (x) {cons (primitive-type-usage x) x}) equals)]
         [longest
          {if (not (empty? withLengths))
              (foldl (lambda (x y) {if (< (car x) (car y)) y x}) (car withLengths) withLengths)
              (cons 1 letter)}]
         [result (cdr longest)])
         result})]
    [_longest-type-equivalent
     (lambda (self type)
       {cond
         [(char? type) (longest-primitive-equivalent type)]
         [(function-type? type)   (cons (self self (car type)) (self self (cdr type)))]})]
    [longest-type-equivalent
     (lambda (type) {_longest-type-equivalent _longest-type-equivalent type})]
    [_smallest-letter-replacement
     (lambda (self equalPairs letter result)
       {cond
         [(empty? equalPairs)             result]
         [(and
           (char? (car (car equalPairs)))
           (char? (cdr (car equalPairs)))
           (or
            (eqv? letter (car (car equalPairs)))
            (eqv? letter (cdr (car equalPairs)))))
          {self self (cdr equalPairs) letter (min-char result (car (car equalPairs)) (cdr (car equalPairs)))}]
         [else
          {self self (cdr equalPairs) letter result}]})]
    [smallest-letter-replacement
     (lambda (letter)
       {_smallest-letter-replacement _smallest-letter-replacement sameTypePairs letter letter})]
    [longest-type-inference
     (lambda (term)
       {let*
        ([termProposals (filter (lambda (proposal) {deep-equal? (car proposal) term}) proposals)]
         [possibleTypes (map (lambda (proposal) {cdr proposal}) termProposals)]
         [typesWithLengths (map (lambda (type) {cons type (primitive-type-usage type)}) possibleTypes)]
         [longestInference
          (foldl
           (lambda (x y) {if (< (cdr x) (cdr y)) y x})
           (car typesWithLengths)
           typesWithLengths)])
         ;{println "ALL PROPOSALS:"}
         ;{println term}
         ;{println proposals}
         
         ;{println "ALL INFERENCES: "}
         ;{println proposals}
         (car longestInference)})]
    [_minify-letters
     (lambda (self type)
       {cond
         [(char? type)
          (smallest-letter-replacement type)]
         [(function-type? type)
          (cons (self self (function-arg-type type)) (self self (function-result-type type)))]
         [else
          (println "itype: ")
          (println type)
          (raise "COULDN'T MINIFY INVALID TYPE!!!")]})]
    [minify-letters
     (lambda (type) {_minify-letters _minify-letters type})]
    [any-type-self-references
     (lambda ()
      {any? sameTypePairs (lambda (pair) {or (contains-primitive? (car pair) (cdr pair)) (contains-primitive? (car pair) (cdr pair))})})])

    {make-initial-proposals term}
    
    ;{println "AFTER INITIAL PROPOSITION:"}
    ;{print-proposals}

    {propose-for-term term}

    ;{println "AFTER RECURSIVE PROPOSITION:"}
    ;{print-proposals}

    {initialize-same-type-pairs}

    ;{println "INITIAL SAME TYPE PAIRS:"}
    ;{print-same-type-pairs}

    {advance-same-type-pairs}


    {if (any-type-self-references) (raise "COULD NOT INFER! SOME TYPES ARE SELF-REFERENCING!") 0}

    ;{println "ADVANCED SAME TYPE PAIRS:"}
    ;{print-same-type-pairs}

    ;{println {smallest-letter-replacement #\θ}}
    ;{print-type {longest-type-equivalent #\α}} 

    {let*
     ([longestDefaultInference (longest-type-inference term)]
      [longestInference (longest-type-equivalent longestDefaultInference)]
      [minified (minify-letters longestInference)])

      ;{print-type longestDefaultInference} {newline}
      ;{print-type longestInference} {newline}
      
      minified}})
