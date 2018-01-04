#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  ;; Editing commands are represented as prefab structs.
  (struct command (name module-path function arguments) #:prefab))

;; The simplest way to attach TODOs is to attach a string to the 'todo syntax
;; property.
(define-syntax (TODO stx)
  (syntax-parse stx
    [(_ msg:str)
     ;; Expand a TODO to a runtime error
     (define runtime
       (syntax/loc stx
         (error msg)))
     ;; Attach a notice that it is a TODO to be filled out
     (syntax-property
      (syntax-property
       runtime
       'todo (syntax->datum #'msg))
      'editing-command (command "Replace with error" "test-command.rkt" 'replace-with-error '()))]))

(define x
  (+ (TODO "one number")
     (TODO "another number")))

(TODO "write more code!")

;; For more control over the placement of TODOs, use the located struct.
;; For the ability to write separate summaries and detailed TODOs, use the todo-item struct.
(begin-for-syntax
  (struct located (loc value) #:prefab)
  (struct todo-item (full summary) #:prefab))

(require racket/stxparam racket/splicing)

;; This example uses a syntax parameter to propagate the surrounding expression
;; context, attaching the todo-item to the context if one exists.
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
    [(_ msg:str)
     (define item
       (located ctx
                (todo-item (syntax->datum #'msg)
                           (syntax-parse ctx
                             #:literals (define/todos)
                             [(define/todos x e) (syntax->datum #'x)]
                             [_ (syntax->datum #'msg)]))))
     (syntax-property (syntax/loc stx (error 'inner-todo msg)) 'todo item)]))

;; The entire definition containing the TODO is highlighted as a thing to be done now.
(define/todos incomplete-fun
  (lambda (y)
    (inner-TODO "This function is incomplete")))


;;; Example of an editing command without a goal.
(define-syntax (with-command stx)
  (syntax-case stx ()
    [(_ e)
     (syntax-property #'e
                      'editing-command
                      (command "Double" "test-command.rkt" 'double '()))]))

(with-command 444)

;; No auto tests here
(module test racket/base)
