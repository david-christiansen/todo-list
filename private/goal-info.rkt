#lang racket

(provide (struct-out goal) (struct-out command) (struct-out todo-item))

;; Alternative goal interface
(struct todo-item (location full summary) #:prefab)

;; Indicate the presence of a goal
(struct goal (full summary) #:prefab)

;; Indicate the potential availability of an interactive editing command
(struct command (name module-path function arguments) #:prefab)
