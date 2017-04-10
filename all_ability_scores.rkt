#lang racket

(define l (stream->list (in-range 3 19)))

;; cost to buy ability score
;; 3 through 6 are not legal values
;; and are extrapolations mirroring the
;; top end of the scale for comparison
;; only
(define (ability->cost n)
  (hash-ref #hash((3 . -16)
                  (4 . -12)
                  (5 . -9)
                  (6 . -6)
                  (7 . -4)
                  (8 . -2)
                  (9 . -1)
                  (10 . 0)
                  (11 . 1)
                  (12 . 2)
                  (13 . 3)
                  (14 . 5)
                  (15 . 7)
                  (16 . 10)
                  (17 . 13)
                  (18 . 17))
            n))

(define (cost-abilities ab)
  (apply + (map ability->cost ab)))

;; get modifier from ability 
(define (ability->modifier n)
  (hash-ref #hash((1 . -5)
                  (2 . -4)
                  (3 . -4)
                  (4 . -3)
                  (5 . -3)
                  (6 . -2)
                  (7 . -2)
                  (8 . -1)
                  (9 . -1)
                  (10 . 0)
                  (11 . 0)
                  (12 . 1)
                  (13 . 1)
                  (14 . 2)
                  (15 . 2)
                  (16 . 3)
                  (17 . 3)
                  (18 . 4)
                  (19 . 4)
                  (20 . 5)
                  (21 . 5)
                  (22 . 6)
                  (23 . 6)
                  (24 . 7)
                  (25 . 7)
                  (26 . 8)
                  (27 . 8)
                  (28 . 9)
                  (29 . 9)
                  (30 . 10)
                  (31 . 10)
                  (32 . 11)
                  (33 . 11)
                  (34 . 12)
                  (35 . 12)
                  (36 . 13)
                  (37 . 13)
                  (38 . 14)
                  (39 . 14)
                  (40 . 15)
                  (41 . 15)
                  (42 . 16)
                  (43 . 16)
                  (44 . 17)
                  (45 . 17))
            n))

(define (rate-abilities ab)
  (apply + (map ability->modifier ab)))

(call-with-output-file "all_scores.csv"  #:exists 'truncate
  (lambda (out)
    (displayln "purchase cost total, modifier total, scores" out)
    (for ([str '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
      (for ([dex '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
        (for ([con '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
          (for ([int '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
            (for ([wis '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
              (for ([chr '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
                (let ([lst (list str dex con int wis chr)])
                  (display (rate-abilities lst) out)
                  (display ", " out)
                  (display (cost-abilities lst) out)
                  (display ", " out)
                  (displayln lst out))))))))))

;; build list of uniq ability scores with cost and rate
(define all_uniq (list->mutable-set '()))
;; brute force build all possible sets of scores in a sorted list
;; but use the characteristics of sets to keep only the unique
;; combinations of cost, rate, and abilities (ignoring the ability order)
(for ([str '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
  (for ([dex '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
    (for ([con '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
      (for ([int '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
        (for ([wis '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
          (for ([chr '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3)])
            (let* ([abils (sort (list str dex con int wis chr) >)]
                   [rate (rate-abilities abils)]
                   [cost (cost-abilities abils)])
              (set-add! all_uniq (list cost rate abils)))))))))

(call-with-output-file "uniq_scores.csv"  #:exists 'truncate
  (lambda (out)
    (displayln "purchase cost total, modifier total, scores" out)
    (for ([item all_uniq])
      (let* ([cost (first item)]
             [rate (second item)]
             [abils (third item)])
        (display cost out)
        (display ", " out)
        (display rate out)
        (display ", " out)
        (displayln abils out)
        ))))

;; build list of uniq ability scores with cost and rate
(define legal_uniq (list->mutable-set '()))
;; brute force build all possible sets of scores in a sorted list
;; but use the characteristics of sets to keep only the unique
;; combinations of cost, rate, and abilities (ignoring the ability order)
(for ([str '(18 17 16 15 14 13 12 11 10 9 8 7)])
  (for ([dex '(18 17 16 15 14 13 12 11 10 9 8 7)])
    (for ([con '(18 17 16 15 14 13 12 11 10 9 8 7)])
      (for ([int '(18 17 16 15 14 13 12 11 10 9 8 7)])
        (for ([wis '(18 17 16 15 14 13 12 11 10 9 8 7)])
          (for ([chr '(18 17 16 15 14 13 12 11 10 9 8 7)])
            (let* ([abils (sort (list str dex con int wis chr) >)]
                   [rate (rate-abilities abils)]
                   [cost (cost-abilities abils)])
              (set-add! legal_uniq (list cost rate abils)))))))))

(call-with-output-file "legal_scores.csv"  #:exists 'truncate
  (lambda (out)
    (displayln "purchase cost total, modifier total, scores" out)
    (for ([item legal_uniq])
      (let* ([cost (first item)]
             [rate (second item)]
             [abils (third item)])
        (display cost out)
        (display ", " out)
        (display rate out)
        (display ", " out)
        (displayln abils out)
        ))))
