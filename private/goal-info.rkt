#lang racket

(provide (struct-out todo-item)
         (struct-out command)
         (struct-out located))

;; A source location is either a syntax object, a srcloc struct, or a
;; list or vector containing the source, line, column, position, and
;; span. See source-location? from syntax/srcloc.

;; Indicate the presence of a todo item
(struct todo-item (full summary) #:prefab)

;; Indicate the potential availability of an interactive editing command
(struct command (name module-path function arguments) #:prefab)

;; Locate a goal or command at a different location from the current syntax.
;; The location should satisfy source-location? from syntax/srcloc.
(struct located (location value) #:prefab)

