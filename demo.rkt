#lang racket
(require (for-syntax syntax/parse))

(define-syntax (TODO stx)
  (syntax-parse stx
    [(_ msg:str)
     ;; Expand a TODO to a runtime error
     (define runtime
       (syntax/loc stx
         (error msg)))
     ;; Attach a notice that it is a goal to be filled out
     (syntax-property
      runtime
      'goal (syntax->datum #'msg))]))

(define x
  (+ (TODO "one number")
     (TODO "another number")))

(TODO "write more code!")
