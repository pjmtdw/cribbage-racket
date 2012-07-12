#lang racket
(require srfi/1 srfi/26 games/cards racket/class)

(provide start-game move-card-aligned net-port net-host net-mode net-obj table)
(define net-port (make-parameter 12345))
(define net-host (make-parameter "localhost"))
(define net-mode (make-parameter #f))
(define net-obj (make-parameter #f))
(define table (make-parameter #f))

; (A A B B B C) -> ((A A) (B B B) (C))
(define (group ls)
 (let loop ([xs (cdr ls)] [ys (list (car ls))] [res '()])
  (cond
   ([null? xs] (reverse (cons ys res)))
   ([equal? (car ys) (car xs)] (loop (cdr xs) (cons (car xs) ys) res))
   (else (loop (cdr xs) (list (car xs)) (cons ys res))))))

; (with-split-list '(a b c d e f) ((x 2) (y 3)) (list y x)) -> ((c d e) (a b)) 
(define-syntax with-split-list
 (syntax-rules ()
  ((_ ls () body ... ) (begin body ...) )
  ((_ ls ((v1 n1) (v2 n2) ...) body ... )
   (let-values ([(v1 rest) (split-at ls n1)]) (with-split-list rest ((v2 n2) ...) body ...)))))

(define player%
 (class object%
  (super-new)
  (init-field [score 0])
  (init-field [cards '()])
  (init-field [is-dealer #f])
  (define/public (append-cards! cds state)
   (set! cards (append cards (map (cut cons <> state) cds))))
  (define/public (remove-cards! cds)
   (define removed (remove (lambda (x) (member (car x) cds)) cards))
   (set! cards removed))
  (define/public (set-cards-state! cds state)
   (remove-cards! cds)
   (append-cards! cds state))
  (define/public (show-card table region)
   (for ([c (map car cards)])
    (send c face-up)
    (send c user-can-move #f)
    )
   (send table add-cards-to-region (map car cards) region))))

(define cards-per-player 6)
(define cribs-per-player 2)

(define (start-game)
 (define cards (shuffle-list (make-deck) 7)); according to doc of games/cards, 7 is sufficient for shuffling list
 (with-split-list cards ([starters 1]
                         [cards1 cards-per-player]
                         [cards2 cards-per-player])
  (let ([pls (map (match-lambda ([list c d] (make-object player% 0 (map (cut cons <> 'have) c) d)))
              (list (list cards1 #t) (list cards2 #f)))])
   (new crib-game% 
    [starter (car starters)]
    [players pls]
    [phase 'select-crib]))))
(define crib-game%
 (class object%
  (super-new)
  (init-field [crib '()])
  (init-field [starter #f])
  (init-field [players #f])
  (init-field [pile '()])
  (init-field [phase #f])
  (define/public (append-pile! card)
   (set! pile (append pile (list card))))
  (define/public (select-crib! player-num cards)
   (define pl (list-ref players player-num))
   (send pl remove-cards! cards)
   (set! crib (append crib cards)))
  (define/public (play-card! player-num card)
   (define pl (list-ref players player-num))
   (send pl set-cards-state! (list card) 'del) 
   (append-pile! card))
  (define/public (show-player-card table region player-num)
   (send (list-ref players player-num) show-card table region))))

; 1 2 3 -> 1
; 1 2 2 3 -> 2
; 1 2 3 3 3 -> 3
; 1 1 2 2 3 -> 4
(define (piles-score-run piles min-run-len)
 (define nums (group (sort (map (cut send <> get-value) piles))))
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
(define (piles-score-comb piles comb-sum)
 (define nums (map (cut send <> get-value) piles))
 (define init-lst (make-list comb-sum 0))
 (last
  (for/fold ([lst init-lst]) ([i nums])
   (define add-from (append (make-list (- i 1) 0) '(1) (take lst (- comb-sum i))))
   (map + add-from lst))
  ))

(define (move-card-aligned table card region maxnum index)
 (define x (+ (region-x region) (* index (/ (region-w region) maxnum))))
 (define y (region-y region))
 (send table move-card card  x y))
  
