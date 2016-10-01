#lang racket

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
                  (45 . 17)) n))

(define (rate-abilities ab)
  (apply + (map ability->modifier ab)))

(define (parse-dice-per-ability string)
  (map string->number (string-split string #px"[,/:]")))

(define PATHFINDERCHARHELP "Try 'pathfinder-character --help' for more information.")
(define generation-method (make-parameter 'standard))
(define verbose-is-on (make-parameter false))
(define number-to-roll (make-parameter 1))
(define dice-per-ability (make-parameter (parse-dice-per-ability "4/4/4/4/4/4")))


;; A dice-rolling command-line utility
(command-line
 ;; remove the following comment to test from DrRacket
 ;; #:argv (list "--classic" "--number" "10" "--verbose")
 ;; #:argv (list "--standard" "--number" "10" "--verbose")
 ;; #:argv (list "--heroic" "--number" "10" "--verbose")
 #:argv (list "--pool" "3:3:4:6:4:4" "--number" "10" "--verbose")
 ;; #:argv (list "--help")
 #:usage-help
 ""
 "where the <arguments> are"
 ""
 "See the --classic, --standard, --heroic and --pool parameters for details."
 ""
 "Examples:"
 ""
 "  pathfinder-character --classic -v --number 10"
 "  pathfinder-character -s -n 3"

 #:once-any
 [("-c" "--classic") ("The classic method: 3D6 per ability.")
                     (generation-method 'classic)]
 [("-s" "--standard") ("The standard method: 4D6 keep high 3 per ability."
                       "(this is the default)")
                      (generation-method 'standard)]
 [("-r" "--heroic") ("The heroic method: 2D6 plus 6 per ability.")
                    (generation-method 'heroic)]
 [("-p" "--pool") perability ("The pool method: 24D6 for all 6 abilities. The parameter"
                              "specifies how many dice are assigned to each ability as"
                              "follows: 3/3/3/3/3/9 with a minimum of 3 dice per ability.")
                  (begin
                    (generation-method 'pool)
                    (dice-per-ability perability))]
 #:once-each
 [("-v" "--verbose") ("Display additional information (default to false).")
                     (verbose-is-on true)]
 [("-n" "--number") n ("Number of characters to roll. Must be greater than 0."
                       "(default to 1)")
                    (number-to-roll (string->number n))]
 
 #:args arguments
 (let* ([dice (cond [(equal? (generation-method) 'classic) 3]
                    [(equal? (generation-method) 'standard) 4]
                    [(equal? (generation-method) 'heroic) 2]
                    [else 1])]
        [keep (cond [(equal? (generation-method) 'heroic) 2]
                    [(equal? (generation-method) 'pool) 1]
                    [else 3])]
        [sides 6]
        [amt (cond [(equal? (generation-method) 'heroic) 6]
                   [else 0])]
        [myrand (lambda (x) (+ 1 (random sides)))]
        [verbose (verbose-is-on)]
        [characters (number-to-roll)]
        [oneability (lambda ()
                      (let* ([rands (build-list dice myrand)]
                             [maxkeep (take (sort rands >) keep)]
                             [sum (apply + maxkeep)]
                             [adjusted (+ sum amt)])
                        adjusted))]
        [numabils (cond [(equal? (generation-method) 'pool) 24]
                         [else 6])]
        [abilities (lambda ()
                     (map (lambda (x) (oneability)) (stream->list (in-range numabils))))]
        [ratings (lambda ()
                   (let* ([a (abilities)]
                          [r (rate-abilities a)])
                     (list a r)))])
   (cond [(< characters 1) (begin
                             (displayln "number of characters must be greater than 0.")
                             (displayln PATHFINDERCHARHELP)) ]
         [(< dice 1) (begin
                       (displayln "dice must be greater than 0.")
                       (displayln PATHFINDERCHARHELP)) ]
         [(< keep 1) (begin
                       (displayln "keep must be greater than 0.")
                       (displayln PATHFINDERCHARHELP)) ]
         [(< dice keep) (begin
                          (displayln "dice must be greater than or equal to keep.")
                          (displayln PATHFINDERCHARHELP)) ]
         [(< sides 1) (begin
                        (displayln "sides must be greater than 0.")
                        (displayln PATHFINDERCHARHELP)) ]
         [else (displayln (sort (map (lambda (x) (ratings)) (stream->list (in-range characters))) (lambda (x y) (< (last x) (last y)))))])
   ))
