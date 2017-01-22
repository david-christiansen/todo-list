#lang racket

(provide (struct-out goal))

(struct goal (full summary) #:prefab)
