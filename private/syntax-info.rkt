#lang racket/base

(require syntax/srcloc)

(provide (struct-out loc-info)
         source-location-loc
         partial-source-location-loc)

(struct loc-info (source position span)
  #:prefab)

(define (source-location-loc stx)
  (loc-info (source-location-source stx)
            (source-location-position stx)
            (source-location-span stx)))

(define (partial-source-location-loc stx)
  (loc-info (syntax-source stx) (syntax-position stx) #f))
