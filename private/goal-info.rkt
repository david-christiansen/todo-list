#lang racket

(provide (struct-out goal) (struct-out command) (struct-out located))

;; A SourceLocation is either a syntax object, a srcloc struct, or a
;; list or vector containing the source, line, column, position, and
;; span. See source-location? from syntax/srcloc.

;; Indicate the presence of a goal
(struct goal (full summary) #:prefab)

;; Indicate the potential availability of an interactive editing command
(struct command (name module-path function arguments) #:prefab)

;; Locate a goal or command at a different location from the current syntax
(struct located (location value) #:prefab)

;; A [Located X] is a (located SourceLocation X)
