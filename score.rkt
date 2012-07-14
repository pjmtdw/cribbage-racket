#lang racket
(require srfi/26)

(provide crib-cards-sum crib-card-value allowed-card-sum hand-score-all pile-score-all)

(define allowed-card-sum 31)
(define middle-bonus 15)
(define min-run-length 3)

; (A A B B B C) -> ((A A) (B B B) (C))
(define (group ls)
 (let loop ([xs (cdr ls)] [ys (list (car ls))] [res '()])
  (cond
   ([null? xs] (reverse (cons ys res)))
   ([equal? (car ys) (car xs)] (loop (cdr xs) (cons (car xs) ys) res))
   (else (loop (cdr xs) (list (car xs)) (cons ys res))))))

(define (hand-score-all starter cards)
 (define cards-with-starter (if starter (cons starter cards) cards))
 `((fifteen . ,(hand-score-fifteen cards-with-starter))
   (pair . ,(hand-score-pair cards-with-starter))
   (run . ,(hand-score-run cards-with-starter))
   (flush . ,(hand-score-flush starter cards))
   (his-nob . ,(hand-score-his-nob starter cards))))

(define (pile-score-all pile)
 `((sum . ,(pile-sum-score pile))
   (run . ,(pile-run-score pile))
   (pair . ,(pile-pair-score pile))))

(define (run-score ps)
 (define nums (sort (map (cut send <> get-value) ps) <))
 (if (for/and ([i nums] [j (drop nums 1)]) (= 1 (- j i)))
  (length ps)
  #f))

; 1 2 3 -> 3
; 1 2 2 3 -> 3*2
; 1 2 3 3 3 -> 3*3
; 1 1 2 2 3 -> 3*4
; 1 2 3 4 4 -> 4*2
(define (hand-score-run-p hands min-run-len)
 (define nums (group (sort (map (cut send <> get-value) hands) <)))
 (let loop ([prev #f] [xs nums] [len 0] [pat 0] [res 0])
  (define next-loop (cut loop (caar xs) (cdr xs) <> <> <> ))
  (define (calc)
   (+ res (* pat (if (>= len min-run-len) len 0))))
  (cond
   ([null? xs] (calc))
   ([or (not prev) (> (- (caar xs) prev) 1)]
    (next-loop 1 (length (car xs)) (calc)))
   (else
    (next-loop (+ len 1) (* pat (length (car xs))) res)))))
; 10 5 -> 1
; 7 8 7 -> 2
; 10 5 5 5 -> 3
; 6 6 9 9 -> 4
(define (hand-score-sum-p hands comb-sum)
 (define nums (map crib-card-value hands))
 (define init-lst (make-list comb-sum 0))
 (last
  (for/fold ([lst init-lst]) ([i nums])
   (define add-from (append (make-list (- i 1) 0) '(1) (take lst (- comb-sum i))))
   (map + add-from lst))))

(define (hand-score-fifteen cards)
 (* 2 (hand-score-sum-p cards middle-bonus)))

(define (hand-score-run cards)
 (hand-score-run-p cards min-run-length))

(define (hand-score-pair cards)
 (define nums (group (sort (map (cut send <> get-value) cards) <)))
 (for/sum ([xs nums])
  (case (length xs)
   ('2 2)
   ('3 6)
   ('4 12)
   (else 0))))

(define (hand-score-flush starter cards)
 (define suits (map (cut send <> get-suit) cards))
 (define starter-suit (if starter (send starter get-suit) #f))
 (define is-flush (= (length (car (group suits))) (length suits)))
 (cond
  ((and is-flush (eq? starter-suit (car suits))) 5)
  (is-flush 4)
  (else 0)))

(define (hand-score-his-nob starter cards)
 (cond
  (starter
   (define starter-suit (send starter get-suit))
   (if (memf (lambda (c) (and (eq? (send c get-suit) starter-suit) (= (send c get-value) 11) )) cards)
  1 0))
  (else 0)))

(define (crib-card-value card)
 (let ([v (send card get-value)])
  (if (> v 10) 10 v)))

(define (crib-cards-sum cards)
 (for/sum ([c cards]) (crib-card-value c)))

(define (pile-sum-score pile)
 (define sum (crib-cards-sum pile))
 (cond
  ((= sum middle-bonus) 2)
  ((= sum allowed-card-sum) 1)
  (else 0)))

(define (pile-run-score pile)
 (or (for/or ([l (in-range (length pile) (- min-run-length 1) -1)])
      (let ([p (take-right pile l)])
       (run-score p))) 0))
(define (pile-pair-score pile)
 (define nums (map (cut send <> get-value) pile))
 (case (length (car (group (reverse nums))))
  ('2 2)
  ('3 6)
  ('4 12)
  (else 0)))
