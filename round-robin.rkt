#lang racket

(require math/statistics)
(require math/number-theory)

; Odds -> Decimal
; Converts US odds to a decimal value
(define (odds-to-decimal odds)
  (/ (if (> odds 0) 100 (abs odds))
     (+ (abs odds) 100)))

; Decimal -> Odds
; Converts a decimal percentage into US odds
(define (decimal-to-odds d)
  (cond
    [(= d 0.5) 100]
    [(< d 0.5) (- (/ 100 d) 100)]
    [(> d 0.5) (/ (* -100 d) (- 1 d))]))

; Nat Nat -> Nat
; combination function (nCr)
(define (c n r)
  ; Returning 0 for invalid args works with where this is used
  (if (> r n) 0
      (/ (factorial n) (* (factorial r) (factorial (- n r))))))

; [Decimal] PosNat -> [Decimal]
; Returns a list containing the odds of each possible parlay of the given size
(define (round-robin-decimals decimal-odds parlay-size)
  (map (λ [l] (apply * l))
             (combinations decimal-odds parlay-size)))

; Dollars Decimal -> Dollars
; Returns the amount a bet of the given amount would win if placed on the specified odds
; This is amount does not include the amount of the bet itself
(define (profit bet decimal)
  (* bet (- (/ 1 decimal) 1)))

; [Odds] Dollars PosNat -> [Dollars]
; Returns the list of profits each possible parlay of the given size would make
; if placed with the specified bet size
(define (round-robin-profits odds bet-size parlay-size)
  (map (λ [dec] (profit bet-size dec))
       (round-robin-decimals (map odds-to-decimal odds) parlay-size)))

; [Odds] Dollars PosNat -> Dollars
; Returns the profit obtained if all parlays of the specified size + bet amount were succesfull
(define (round-robin-max-profit odds bet-size parlay-size)
  (foldr + 0 (round-robin-profits odds bet-size parlay-size)))

; [Odds] Dollars PosNat Nat -> Dollars
; Returns the profit obtained (negative profit equals loss)
; on average if a single game in the parlay misses
;
; Treats each game as equally likely to lose, as the author of this file
; does not consider sportsbook odds as a reliable measure of probality, as they
; speak more to the opinions of the betting public & the sportsbook's attempt to
; financially benefit in either outcome.
(define (round-robin-average-profit-n-losses odds bet-size parlay-size losses)
  ; Average return from winning bets
  (- (mean (map (λ [odds] (round-robin-max-profit odds bet-size parlay-size))
                (combinations odds (- (length odds) losses))))
     ; Amount lost on losing bets
     (* bet-size
        ; Number of lost bets is number of total bets minus bets won
        (- (c (length odds) parlay-size) (c (- (length odds) losses) parlay-size)))))

; [Odds] Dollars [PosNat] Nat -> Dollars
; Performs the calculation described above for a given list of various parlay sizes
(define (round-robin-average-profit-n-losses-multi odds bet-size parlay-sizes losses)
  (foldr (λ [parlay-size profit-sum]
           (+ profit-sum (round-robin-average-profit-n-losses odds bet-size parlay-size losses)))
         0 parlay-sizes))

; [Odds] Dollars [PosNat] [Nat] -> '(Amount Bet, Max Profit, 1 Loss Profit)
; Performs the calculation described above for both the 0 and 1 loss scenarios
;
; These two scenarios are of particular interest as they represent the outcomes for which
; a confident gambler would try to optimize
(define (round-robin-stats odds bet-size parlay-sizes)
  (cons (foldr (λ [parlay-size bet-sum] (+ bet-sum
                                          (* bet-size (c (length odds) parlay-size))))
              0 parlay-sizes)
        (map (λ [losses] (round-robin-average-profit-n-losses-multi odds bet-size parlay-sizes losses))
            '(0 1))))


; [Odds] -> ['(Parlay Sizes, Total Bet, Max Profit, 1 Loss Profit)]
; Performs the calculation described above for all combinations of parlay sizes
(define (round-robin-data odds)
  (map (λ [parlay-sizes]
         (cons parlay-sizes (round-robin-stats odds 1 parlay-sizes)))
       ; All combinations of parlay sizes, minus empty list
       (cdr (combinations (map add1 (cdr (range (length odds))))))))

;-----------------------------------------------

; [Odds] -> void
; Prints the results of the calculation above in a readable format
(define (round-robin-print odds)
  (displayln "Parlay Sizes\tTotal Bet\tMax Profit\t1 Loss Profit")
  (for ([entry (round-robin-data odds)])
    (match entry
      [(list parlay-sizes total-bet max-profit 1-loss-profit)
       (map display (list
                     parlay-sizes
                     "\t\t$" (~r total-bet)
                     "\t\t$" (~r max-profit)
                     "\t" (if (> 1-loss-profit 0) "$" "-$") (~r (abs 1-loss-profit))))
       (newline)])))
  

;-----------------------------------------------

(define week3-picks '(-300 -1500 -560 -330))
(define week4-picks '(-420 -320 -134 -600))
(define week4-picks-2 '(-320 -130 -590 -154))
(define week5-picks '(-400 -370 -330 -174 -1200))
(define week5-picks+chargers (cons -190 week5-picks))
(define week7 '(-295 -420 -1200 -215))

;-----------------------------------------------

(round-robin-print week7)