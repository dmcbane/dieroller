#lang racket

;; parse the modifier
(define (parse-modifier modifier)
  (let* ([s (string-trim modifier)]
         [first (substring s 0 1)]
         [value (string-trim (substring s 1))])
    (cond [(equal? first "+") (list + (string->number value) (string-append "+" value))]
          [(equal? first "-") (list - (string->number value) (string-append "-" value))]
          [(equal? first "*") (list * (string->number value) (string-append "*" value))]
          [else (list + (string->number (string-append first value)) (string-append "+" first value))])))


(define DIEROLLERHELP "Try 'dieroller --help' for more information.")
(define number-to-roll (make-parameter 1))
(define sides-per-die (make-parameter 20))
(define number-to-keep (make-parameter 0))
(define modifier-to-rolls (make-parameter "0"))

;; A dice-rolling command-line utility
(command-line
 ;; remove the following comment to test from DrRacket
 ;; #:argv (list "5")
 ;; #:argv (list "1" "10")
 ;; #:argv (list "3" "6" "+3")
 ;; #:argv (list "3" "6" "+6" "2")
 ;; #:argv (list "--keep" "2" "--dice" "3" "--modifier" "+6" "--sides" "6")
 ;; #:argv (list "--dice" "4" "--sides" "6" "--keep" "3")
 ;; #:argv (list "--keep" "2" "--dice" "1" "--modifier" "+6" "--sides" "6")
 ;; #:argv (list "--help")
 #:usage-help
 ""
 "where the <arguments> are"
 ""
 "  <dice>"
 "or"
 "  <dice> <sides>"
 "or"
 "  <dice> <sides> <modifier>" 
 "or"
 "  <dice> <sides> <modifier> <keep>" 
 ""
 "See the --dice, --sides, and --modifier parameters for details."
 ""
 "Examples:"
 ""
 "  dieroller 5"
 "  dieroller 1 10"
 "  dieroller 3 6 +3"
 "  dieroller 3 6 +6 2"
 "  dieroller --dice 5 --sides 100 --modifier +4 --keep 3"
 "  dieroller --dice 4 --sides 6 --keep 3"
 ""
 #:once-each
 [("-d" "--dice") dice ("Number of dice to roll.  Must be greater than 0."
                        "(default to 1)")
                  (number-to-roll (string->number dice))]
 [("-k" "--keep") keep
                  ("Number of rolls to keep. Must be greater than 0 and less than or equal to <dice>."
                   "(default to number of dice)")
                  (number-to-keep (string->number keep))]
 [("-m" "--modifier") modifier
                      ("Modifier to the rolls. The first character can optionally"
                       "be one of +, -, or * followed by a number.  If the +, -, or"
                       "* are missing, + is assumed. (default to no modifier)")
                      (modifier-to-rolls modifier)]
 [("-s" "--sides") sides
                   ("Number of sides per die. Must be greater than 0."
                    "(default to 20)")
                   (sides-per-die (string->number sides))]
 #:args arguments
 (let* ([dice (if (< (length arguments) 1)
                  (number-to-roll)
                  (string->number (first arguments)))]
        [keep (if (< (length arguments) 4)
                  (if (< (number-to-keep) 1)
                      dice
                      (number-to-keep))
                  (string->number (fourth arguments)))]
        [sides (if (< (length arguments) 2)
                   (sides-per-die)
                   (string->number (second arguments)))]
        [modifier (parse-modifier (if (< (length arguments) 3)
                                      (modifier-to-rolls)
                                      (third arguments)))]
        [op (first modifier)]
        [amt (second modifier)]
        [dicetype (string-append (number->string dice)
                                 "D"
                                 (number->string sides)
                                 (if (equal? (third modifier) "+0")
                                     ""
                                     (third modifier))
                                 (if (= dice keep)
                                     ""
                                     (string-append " keep " (number->string keep))))]
        [myrand (lambda (x) (+ 1 (random sides)))])
   (cond [(< dice 1) (begin
                       (displayln "dice must be greater than 0.")
                       (displayln DIEROLLERHELP)) ]
         [(< keep 1) (begin
                       (displayln "keep must be greater than 0.")
                       (displayln DIEROLLERHELP)) ]
         [(< dice keep) (begin
                          (displayln "dice must be greater than or equal to keep.")
                          (displayln DIEROLLERHELP)) ]
         [(< sides 1) (begin
                        (displayln "sides must be greater than 0.")
                        (displayln DIEROLLERHELP)) ]
         [else (let* ([rands (build-list dice myrand)]
                      [maxkeep (take (sort rands >) keep)]
                      [sum (apply + maxkeep)]
                      [adjusted (op sum amt)])
                 (displayln dicetype)
                 (display "Result: ")
                 (if (> (length rands) 1)
                     (begin
                       (display "(")
                       (display (string-join (map number->string rands) " "))
                       (display ") "))
                     (null? null))
                 (display "=> ")
                 (displayln adjusted)
                 )])
   ))