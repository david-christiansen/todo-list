#lang racket

(provide build-todo-info handle-expansion)

(require syntax/parse syntax/srcloc
         "syntax-info.rkt" "goal-info.rkt")

;; traverse fully expanded syntax and produce a list suitable for building
;; an interval-map. This maps intervals of todos to their metadata.
(define (build-todo-info stx source)
  (define todo-info (make-hash))
  (define command-info (make-hash))
  (let loop ([stx stx])
    ;; this-todo is either #f or a located todo
    (define these-todos (detect-todos stx))
    ;; this-command is a list of located commands
    (define these-commands (detect-commands stx))

    (for ([this-todo (in-list these-todos)])
      (when (equal? source (located-source this-todo))
        (hash-update! todo-info
                      (source-location-loc (located-location this-todo))
                      (λ (old)
                        ;; If we encounter precisely the same source
                        ;; span, then ditch the todos if they're not the
                        ;; same.
                        (cond
                          [old
                           (match-define (todo-item old-todo old-summary) old)
                           (match-define (todo-item new-todo summary)
                             (located-value this-todo))
                           (todo-item (and (equal? old-todo new-todo) old-todo)
                                      (and (equal? old-summary summary) old-summary))]
                          [else (located-value this-todo)]))
                      #f)))

    (for ([this-command (in-list these-commands)]
          #:when (equal? source (located-source this-command)))
      (hash-update! command-info
                    (source-location-loc (located-location this-command))
                    (λ (old)
                      (cond [(not old) (list (located-value this-command))]
                            [(member (located-value this-command) old) old]
                            [else (cons (located-value this-command) old)]))
                    #f))
    (when (syntax->list stx)
      (for ([sub-stx (in-syntax stx)])
        (loop sub-stx))))
  (list (build-pre-interval-map/todo todo-info)
        (build-pre-interval-map/commands command-info)))

;; Construct a list that is suitable input to make-interval-map
(define (build-pre-interval-map/todo table)
  (sort (for*/list ([(k v) (in-hash table)]
                    [full (in-value (todo-item-full v))]
                    #:when full
                    [summary (in-value (todo-item-summary v))])
          (match-define (loc-info _ pos span) k)
          (cons (cons (sub1 pos) (sub1 (+ pos span)))
                v))
        (match-lambda**
         [((cons start1 end1) (cons start2 end2))
          (or (< start1 start2)
              (and (= start1 start2)
                   (>= end1 end2)))])
        #:key car))

(define (build-pre-interval-map/commands table)
  (sort (for*/list ([(k v) (in-hash table)])
          (match-define (loc-info _ pos span) k)
          (define where (cons (sub1 pos) (sub1 (+ pos span))))
          (cons where v))
        (match-lambda**
         [((cons start1 end1) (cons start2 end2))
          (or (< start1 start2)
              (and (= start1 start2)
                   (>= end1 end2)))])
        #:key car))

(define (handle-expansion stx path source cust)
  (and (syntax? stx)
       (build-todo-info stx source)))


;; ------------------------------------------------------------------------


;; flatten* : (-> (-> Any Boolean) Any (Listof Any))
;; Flatten and filter a cons tree of values into a list of values accepted by ok?
(define (flatten* ok? v)
  (cond
    [(ok? v) (list v)]
    [(pair? v) (append (flatten* ok? (car v)) (flatten* ok? (cdr v)))]
    [else '()]))

;; add-location : (∀ (A) (-> Loc (-> A (Located A))))
;; Add a location to an object if it doesn't already have one.
(define ((add-location loc) v)
  (if (located? v) v (located loc v)))

(define (todoify v)
  (cond
    [(todo-item? v) v]
    [(string? v) (todo-item v v)]
    [else (error 'todoify "Bad todo: ~v" v)]))

;; map-location : (∀ (A B) (-> (-> A B) (Located A) (Located B)))
;; Locations are functorial
(define (map-location f loc)
  (match loc
    [(located where what) (located where (f what))]))

;; perhaps-located? : (-> (-> Any Boolean) (-> Any Boolean))
;; Check whether a value satisfies a predicate or is a located value
;; satsifying the predicate.
(define ((perhaps-located? ok?) x)
  (or (ok? x) (and (located? x) (ok? (located-value x)))))


;; detect-todos : (-> Syntax (Listof (Located Todo)))
;; The todo for a syntax object is in the 'todo syntax property. It consists of a
;; cons tree of strings or todo-item structures, possibly located.
(define (detect-todos stx)
  (define todos (syntax-property stx 'todo))
  (if (false? todos)
      '()
      (for/list ([todo (in-list (flatten* (perhaps-located?
                                           (lambda (x)
                                             (or (string? x)
                                                 (todo-item? x))))
                                          todos))])
        (map-location todoify ((add-location stx) todo)))))


;; detect-command : (-> Syntax (Listof (Located Command)))
;; The command or commands for a syntax object are in the 'editing-command syntax
;; property.
(define (detect-commands stx)
  (define these-commands (syntax-property stx 'editing-command))
  (if (false? these-commands)
      '()
      (map (add-location stx)
           (flatten* (perhaps-located? command?)
                     these-commands))))


;; located-source : (-> (Located X) Any)
(define (located-source lctd)
  (source-location-source (located-location lctd)))

