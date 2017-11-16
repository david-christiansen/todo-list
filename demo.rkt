#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  (struct command (name module-path function arguments) #:prefab))

(define-syntax (TODO stx)
  (syntax-parse stx
    [(_ msg:str)
     ;; Expand a TODO to a runtime error
     (define runtime
       (syntax/loc stx
         (error msg)))
     ;; Attach a notice that it is a goal to be filled out
     (syntax-property
      (syntax-property
       runtime
       'goal (syntax->datum #'msg))
      'editing-command (command "Replace with error" "test-command.rkt" 'replace-with-error '()))]))

(define x
  (+ (TODO "one number")
     (TODO "another number")))

(TODO "write more code!")

(define-syntax (with-command stx)
  (syntax-case stx ()
    [(_ e) (syntax-property #'e 'editing-command (command "Double" "test-command.rkt" 'double '()))]))

(with-command 444)

