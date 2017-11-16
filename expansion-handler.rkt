#lang racket

(provide build-goal-info handle-expansion)

(require syntax/parse
         "syntax-info.rkt" "goal-info.rkt")

;; traverse fully expanded syntax and produce a list suitable for building
;; an interval-map. This maps intervals of goals to their metadata.
(define (build-goal-info stx source)
  (define goal-info (make-hash))
  (define command-info (make-hash))
  (let loop ([stx stx])
    (define full-goal (syntax-property stx 'goal))
    ;; The summary is optional
    (define summary (or (syntax-property stx 'goal-summary) full-goal))
    ;; Detect commands
    (define this-command (syntax-property stx 'editing-command))

    (when (and full-goal (equal? source (syntax-source stx)))
      (hash-update! goal-info
                    (syntax-loc stx)
                    (λ (old)
                      ;; If we encounter precisely the same source
                      ;; span, then ditch the goals if they're not the
                      ;; same.
                      (cond
                        [old
                         (match-define (goal old-goal old-summary) old)
                         (goal (and (equal? old-goal full-goal) old-goal)
                               (and (equal? old-summary summary) old-summary))]
                        [else (goal full-goal summary)]))
                    #f))

    (when (and this-command (equal? source (syntax-source stx)))
      (hash-update! command-info
                    (syntax-loc stx)
                    (λ (old)
                      (cond [(not old) (list this-command)]
                            [(member this-command old) old]
                            [else (cons this-command old)]))
                    #f))
    (when (syntax->list stx)
      (for ([sub-stx (in-syntax stx)])
        (loop sub-stx))))
  (displayln command-info)
  (list (build-pre-interval-map/goal goal-info)
        (build-pre-interval-map/commands command-info)))

;; Construct a list that is suitable input to make-interval-map
(define (build-pre-interval-map/goal table)
  (sort (for*/list ([(k v) (in-hash table)]
                    [full (in-value (goal-full v))]
                    #:when full
                    [summary (in-value (goal-summary v))])
          (match-define (syntax-info _ pos span) k)
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
          (displayln `(v ,v))
          (match-define (syntax-info _ pos span) k)
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
