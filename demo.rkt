#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  ;; Editing commands are represented as prefab structs.
  (struct command (name module-path function arguments) #:prefab))

;; The simplest way to attach goals is to attach a string to the 'goal syntax
;; property.
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

;; For more control over the placement of goals, use the todo-item struct.
;; This example uses a syntax parameter to propagate the surrounding expression
;; context, attaching the todo-item to the context if one exists.
(begin-for-syntax
  (struct located (loc value) #:prefab)
  (struct goal (full summary) #:prefab))

(require racket/stxparam racket/splicing)

(define-syntax-parameter definition-context #f)

(define-syntax (define/todos stx)
  (syntax-parse stx
    [(_ x e)
     (with-syntax ([ctx stx])
       (syntax/loc stx
         (splicing-syntax-parameterize ([definition-context #'ctx])
           (define x e))))]))

(define-syntax (inner-TODO stx)
  (define ctx (or (syntax-parameter-value #'definition-context) stx))
  (syntax-parse stx
    [(_ msg:string)
     (define item (located ctx (goal (syntax->datum #'msg) #f)))
     (syntax-property (syntax/loc stx (error 'inner-todo msg)) 'goal item)]))

;; The entire definition containing the TODO is highlighted as a goal now.
(define/todos incomplete-fun
  (lambda (y) (inner-TODO "hello")))


;;; Example of an editing command without a goal.
(define-syntax (with-command stx)
  (syntax-case stx ()
    [(_ e) (syntax-property #'e
                            'editing-command
                            (command "Double" "test-command.rkt" 'double '()))]))

(with-command 444)

;; No auto tests here
(module test racket/base)
