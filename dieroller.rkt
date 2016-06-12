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


(define DIEROLLERHELP "Try 'dieroller --help' for more information.")(define number-of-rolls-to-keep (make-parameter null))
(define number-to-keep (make-parameter 0))

;; A dice-rolling command-line utility
(command-line
 ;; remove the following comment to test from DrRacket
 ;; #:argv (list "6" "6" "+3")
 #:usage-help
 ""
 "where the <arguments> are [dice] [sides] and [modifier]"
 "  [dice] must be a number greater than 0."
 "  [sides] must be a number greater than 0."
 "  [modifier] is optional.  It can be an optional +, -, or * followed by a number." 
 "    If the +, -, or * is missing, + is assumed."
 ""
 "Examples:"
 ""
 "  dieroller 1 10"
 "  dieroller 3 6 +6"
 "  dieroller 2 100 -4"
 ""
 #:once-each
 [("-k" "--keep") number
                  ("Number of rolls to keep"
                   "(default to all)")
                  (number-to-keep (string->number number))]
 #:args arguments
 (let* ([dice (if (< (length arguments) 1) 1 (string->number (first arguments)))]
        [keep (if (< (number-to-keep) 1) dice (number-to-keep))]
        [sides (if (< (length arguments) 2) 20 (string->number (second arguments)))]
        [modifier (parse-modifier (if (< (length arguments) 3) "0" (third arguments)))]
        [op (first modifier)]
        [amt (second modifier)]
        [dicetype (string-append (number->string dice) "D" (number->string sides) " " (third modifier) " keep " (number->string keep))]
        [myrand (lambda (x) (+ 1 (random sides)))])
   (cond [(< dice 1) (begin
                       (displayln "dice must be greater than 0.")
                       (displayln DIEROLLERHELP)) ]
         [(< sides 1) (begin
                        (displayln "sides must be greater than 0.")
                        (displayln DIEROLLERHELP)) ]
         [else (let* ([rands (build-list dice myrand)]
                      [maxkeep (take (sort rands >) keep)]
                      [sum (apply + maxkeep)]
                      [adjusted (op sum amt)])
                 (displayln dicetype)
                 (display "Result: (")
                 (display (string-join (map number->string rands) " "))
                 (display ") => ")
                 (displayln adjusted)
                 )])
   ))