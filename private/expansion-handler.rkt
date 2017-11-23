#lang racket

(provide build-goal-info handle-expansion)

(require syntax/parse syntax/srcloc
         "syntax-info.rkt" "goal-info.rkt")

;; traverse fully expanded syntax and produce a list suitable for building
;; an interval-map. This maps intervals of goals to their metadata.
(define (build-goal-info stx source)
  (define goal-info (make-hash))
  (define command-info (make-hash))
  (let loop ([stx stx])
    ;; this-goal    : [Maybe [Located Goal]]
    ;; this-command : [Maybe [Located Command]]
    (define this-goal (detect-goal stx))
    (define this-command (detect-command stx))

    (when (and this-goal (equal? source (located-source this-goal)))
      (hash-update! goal-info
                    (source-location-loc (located-location this-goal))
                    (λ (old)
                      ;; If we encounter precisely the same source
                      ;; span, then ditch the goals if they're not the
                      ;; same.
                      (cond
                        [old
                         (match-define (goal old-goal old-summary) old)
                         (match-define (goal new-goal summary)
                           (located-value this-goal))
                         (goal (and (equal? old-goal new-goal) old-goal)
                               (and (equal? old-summary summary) old-summary))]
                        [else (located-value this-goal)]))
                    #f))

    (when (and this-command (equal? source (located-source this-command)))
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
  (list (build-pre-interval-map/goal goal-info)
        (build-pre-interval-map/commands command-info)))

;; Construct a list that is suitable input to make-interval-map
(define (build-pre-interval-map/goal table)
  (sort (for*/list ([(k v) (in-hash table)]
                    [full (in-value (goal-full v))]
                    #:when full
                    [summary (in-value (goal-summary v))])
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
       (build-goal-info stx source)))


;; ------------------------------------------------------------------------

;; detect-goal : Syntax -> [Maybe [Located Goal]]
;; The goal for a syntax object is either:
;;  - the 'goal syntax property, if it is already a goal structure or
;;    located goal structure
;;  - the 'goal property combined with the 'goal-summary property
(define (detect-goal stx)
  (define full-goal (syntax-property stx 'goal))
  (cond
    [(false? full-goal) #false]
    [(goal? full-goal) (located stx full-goal)]
    [(located? full-goal)
     (if (goal? (located-value full-goal))
         full-goal
         #false)]
    [else
     ;; The summary is optional
     (define summary (or (syntax-property stx 'goal-summary) full-goal))
     ;; The goal structure includes stx for the source location
     (located stx (goal full-goal summary))]))

;; detect-command : Syntax -> [Maybe [Located Command]]
;; The command for a syntax object is in the 'editing-command syntax
;; property.
(define (detect-command stx)
  (define this-command (syntax-property stx 'editing-command))
  (cond
    [(false? this-command) #false]
    [(command? this-command) (located stx this-command)]
    [(located? this-command)
     (if (command? (located-value this-command))
         this-command
         #false)]
    [else #false]))


;; located-source : [Located X] -> Any
(define (located-source lctd)
  (source-location-source (located-location lctd)))

