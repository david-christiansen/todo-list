#lang racket/base

(define (double #:string str)
  (string-append str str))

(define (replace-with-error)
  "(error 'todo)")

(provide double replace-with-error)
