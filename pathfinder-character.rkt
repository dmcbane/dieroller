#lang racket

(require memoize)

;; cost to buy ability score
(define (ability->cost n)
  (hash-ref #hash((7 . -4)
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

(define (parse-dice-per-ability string)
  (map string->number (string-split string #px"[,/:]")))

;; total ability purchase points available by campaign type
(define (campaign-type->total-purchase-points n)
  (cond [(equal? 'low n) 10]
        [(equal? 'standard n) 15]
        [(equal? 'high n) 20]
        [(equal? 'epic n) 25]
        [else 0]))

(define (parse-purchase-type string)
  (let* ([s (string-upcase(string-trim string))])
    (cond [(regexp-match? #rx"^E" s) 'epic]
          [(regexp-match? #rx"^H" s) 'high]
          [(regexp-match? #rx"^S" s) 'standard]
          [else 'low])))

(define (attribute-generator dice keep sides modamt)
  (let* ([myrand (lambda (x) (+ 1 (random sides)))])
    (lambda ()
      (let* ([rands (build-list dice myrand)]
             [maxkeep (take (sort rands >) keep)]
             [sum (apply + maxkeep)]
             [adjusted (+ sum modamt)])
        adjusted))))

(define/memo (legal-purchase-uniq-sets)
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
  (for/list ([i legal_uniq]) i))

(define (purchase-generator points-available)
  (lambda ()
    (first
     (shuffle
      (map (lambda (cost-rate-abils) (third cost-rate-abils))
           (filter (lambda (cost-rate-abils) (equal? points-available (first cost-rate-abils))) (legal-purchase-uniq-sets)))))))

(define (method->dice m)
  (cond [(equal? m 'heroic) 2]
        [(equal? m 'classic) 3]
        [else 4]))

(define (method->keep m)
  (cond [(equal? m 'heroic) 2]
        [else 3]))

(define (method->adjustment-amount m)
  (cond [(equal? m 'heroic) 6]
        [else 0]))

(define PATHFINDERCHARHELP "Try 'pathfinder-character --help' for more information.")
(define generation-method (make-parameter 'standard))
(define verbose-is-on (make-parameter false))
(define number-to-roll (make-parameter 1))
(define dice-per-ability (make-parameter (parse-dice-per-ability "4/4/4/4/4/4")))
(define purchase-points (make-parameter (campaign-type->total-purchase-points (parse-purchase-type "standard"))))


;; A pathfinder character generation command-line utility
(command-line
 ;; remove the following comment to test from DrRacket
 ;; #:argv (list "--classic" "--number" "10" "--verbose")
 ;; #:argv (list "--standard" "--number" "10" "--verbose")
 ;; #:argv (list "--heroic" "--number" "10" "--verbose")
 ;; #:argv (list "--pool" "3:3:4:6:4:4" "--number" "10" "--verbose")
 ;; #:argv (list "-p" "epic" "-n" "10" "-v")
 ;; #:argv (list "--help")
 
 #:usage-help
 ""
 "Examples:"
 ""
 "  pathfinder-character --classic -v --number 10"
 "  pathfinder-character -s -n 3"
 ""
 
 #:once-any
 [("-c" "--classic") ("The classic method: 3D6 per ability.")
                     (generation-method 'classic)]
 [("-s" "--standard") ("The standard method: 4D6 keep high 3 per ability."
                       "(this is the default)")
                      (generation-method 'standard)]
 [("-r" "--heroic") ("The heroic method: 2D6 plus 6 per ability.")
                    (generation-method 'heroic)]
 [("-l" "--pool") diceperability ("The pool method: 24D6 for all 6 abilities. The parameter"
                                  "specifies how many dice are assigned to each ability as"
                                  "follows: 3/3/3/3/3/9 with a minimum of 3 dice per ability.")
                  (begin
                    (generation-method 'pool)
                    (dice-per-ability (parse-dice-per-ability diceperability)))]
 [("-p" "--purchase") purchasetype ("The purchase method: parameters are set according to cost."
                                    "The parameter specifies the purchase type as follows: low,"
                                    "standard, high, and epic fantasy which provides 10, 15, 20,"
                                    "and 25 purchase points respectively.")
                      (begin
                        (generation-method 'purchase)
                        (purchase-points (campaign-type->total-purchase-points (parse-purchase-type purchasetype))))]
 
 #:once-each
 [("-v" "--verbose") ("Display additional information (default to false).")
                     (verbose-is-on true)]
 [("-n" "--number") n ("Number of characters to roll. Must be greater than 0."
                       "(default to 1)")
                    (number-to-roll (string->number n))]
 
 #:args arguments
 
 (let* ([pool-dist (dice-per-ability)]
        [dice (method->dice (generation-method))]
        [keep (method->keep (generation-method))]
        [sides 6]
        [numabils 6]
        [amt (method->adjustment-amount (generation-method))]
        [verbose (verbose-is-on)]
        [characters (number-to-roll)])
   ;; (lambda () (map (lambda (x) (oneability)) (stream->list (in-range numabils))))))
   ;; (lambda () (let* ([a (abilities)]) (list a (rate-abilities a))))))
   (cond [(< characters 1) (begin
                             (displayln "number of characters must be greater than 0.")
                             (displayln PATHFINDERCHARHELP)) ]
         [(not (equal? (length pool-dist) 6)) (begin
                                                (displayln "dice per attribute must specify die quantity for six attributes.")
                                                (displayln PATHFINDERCHARHELP)) ]
         [(ormap (lambda (x) (< x 3)) pool-dist) (begin
                                                   (displayln "a minimum of 3 dice must be used for each attribute.")
                                                   (displayln PATHFINDERCHARHELP)) ]
         [(not (equal? (for/sum ([x pool-dist]) x) 24)) (begin
                                                          (displayln "you must specify a total of twenty-four dice for the pool.")
                                                          (displayln PATHFINDERCHARHELP)) ]
         [else (let* ([ability-gen (cond [(equal? (generation-method) 'pool) (map (lambda (cnt) (attribute-generator cnt keep sides amt)) pool-dist)]
                                         [(equal? (generation-method) 'purchase) 'nil]
                                         [else (map (lambda (x) (attribute-generator dice keep sides amt)) (stream->list (in-range numabils)))])]
                      [abilities (cond [(equal? (generation-method) 'purchase) (purchase-generator (purchase-points))]
                                       [else (lambda () (map (lambda (x) (x)) ability-gen))])]
                      [with-ratings (lambda (x) (let* ([a (abilities)]) (list a (rate-abilities a))))]
                      [all-characters (sort (map with-ratings (stream->list (in-range characters))) (lambda (x y) (< (last x) (last y))))])
                 (if verbose
                     (map (lambda (char-with-rating)
                            (let ([attrs (first char-with-rating)]
                                  [rating (second char-with-rating)])
                              (let ([str (first attrs)]
                                    [dex (second attrs)]
                                    [con (third attrs)]
                                    [int (fourth attrs)]
                                    [wis (fifth attrs)]
                                    [chr (sixth attrs)])
                                (display "STR: ")
                                (display str)
                                (display " DEX: ")
                                (display dex)
                                (display " CON: ")
                                (display con)
                                (display " INT: ")
                                (display int)
                                (display " WIS: ")
                                (display wis)
                                (display " CHR: ")
                                (display chr)
                                (display " (")
                                (display rating)
                                (displayln ")")
                                attrs)))
                          all-characters)
                     (displayln (map (lambda (char-with-rating) (first char-with-rating)) all-characters))))])))
