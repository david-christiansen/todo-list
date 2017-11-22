#lang info

(define collection "todo-list")
(define version "0.4")
(define pkg-desc "An interactive todo list for DrRacket")

(define drracket-name "Todo List")
(define drracket-tools (list (list "tool.rkt")))
(define drracket-tool-names (list "Todo List"))

(define deps '("base"
               "data-lib"
               "drracket-plugin-lib"
               "gui-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/todo-list.scrbl" ())))

(define test-omit-paths '("demo.rkt"))
