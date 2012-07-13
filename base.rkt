#lang racket
(require srfi/1 srfi/26 games/cards racket/class)

(provide start-game net-port net-host net-mode net-obj
 card->hash hash->card cribs-per-player number-of-cribs cribs-per-player total-hands)

(define net-port (make-parameter 12345))
(define net-host (make-parameter "localhost"))
(define net-mode (make-parameter #f))
(define net-obj (make-parameter #f))

(define cards-per-player 6)
(define cribs-per-player 2)
(define number-of-players 2)
(define number-of-cribs (* number-of-players cribs-per-player))
(define hands-per-player (- cards-per-player cribs-per-player))
(define total-hands (* number-of-players hands-per-player))
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
  (define/public (have-card? card)
   (memf (lambda (x) (equal? card (car x))) cards))
  (define/public (append-cards! cds state)
   (set! cards (append cards (map (cut cons <> state) cds))))
  (define/public (remove-cards! cds)
   (define removed (remove (lambda (x) (member (car x) cds)) cards))
   (set! cards removed))
  (define/public (add-score dx)
   (set! score (+ score dx)))
  (define/public (get-score)score)
  (define/public (set-cards-state! cds state)
   (remove-cards! cds)
   (append-cards! cds state))
  (define/public (cards-have)
   (map car (filter (lambda (x) (eq? (cdr x) 'have)) cards)))
  (define/public (all-face-up table)
   (send table cards-face-up (map car cards)))
  (define/public (playable? pile)
   (define ch (cards-have))
   (if (null? ch) #f
    (begin
     (let ([sum (crib-cards-sum pile)]
           [smallest (apply min (map crib-card-value ch))])
      (<= smallest (- allowed-card-sum sum))))))
  (define/public (move-player-card-to-region table region)
   (send table move-cards-to-region (map car cards) region))
  (define/public (show-card table region-from region-to do-face-up)
   (define cds (map car cards))
   (for ([c cds])
    (send c user-can-move #f)
    )
   (send table add-cards-to-region cds region-from)
   (send table move-cards-to-region cds region-to)
   (when do-face-up (send table cards-face-up cds)))))

(define cards-hash #f)

; Since card<%> is not serializable, we store the hash code of all the cards
(define (card->hash card) (list (send card get-suit) (send card get-value)))
(define (hash->card hash) (hash-ref cards-hash hash))
(define (make-cards-hash cards)
 (set! cards-hash (make-hash (for/list ([c cards]) (cons (card->hash c) c)))))

(define (start-game)
 (define cards (shuffle-list (make-deck) 7)); according to doc of games/cards, 7 is sufficient for shuffling list
 (make-cards-hash cards)
 (with-split-list cards ([starters 1]
                         [cards1 cards-per-player]
                         [cards2 cards-per-player])
  (let ([pls (map (lambda (c) (make-object player% 0 (map (cut cons <> 'have) c)))
              (list cards1 cards2))])
   (new crib-game%
    [starter (car starters)]
    [players pls]
    ))))

(define-syntax with-player-num
 (syntax-rules ()
  ((_ () body ...) (begin body ...))
  ((_ ((pl player-num) rest ... ) body ...)
   (let ([pl (list-ref (get-field players this) player-num)]) (with-player-num (rest ...) body ...)))))

(define-syntax define/delegate-playernum
 (syntax-rules ()
  ((_ (func player-num rest ...))
   (define/public (func player-num rest ...)
    (with-player-num ((pl player-num))
      (send pl func rest ...))))))

(define crib-game%
 (class object%
  (super-new)
  (init-field [crib '()])
  (init-field [starter #f])
  (init-field [players #f])
  (init-field [pile '()])
  (define/public (hand-score player-num)
   (define cards 
    (if player-num
     (with-player-num ((pl player-num))
      (map car (get-field cards pl)))
     crib))
   (define cards-with-starter (cons starter cards))
   `((fifteen . ,(hand-score-fifteen cards-with-starter))
     (pair . ,(hand-score-pair cards-with-starter))
     (run . ,(hand-score-run cards-with-starter))
     (flush . ,(hand-score-flush starter cards))
     (his-nob . ,(hand-score-his-nob starter cards))))
  (define/public (pile-score)
   (+ (pile-sum-score) (pile-run-score) (pile-pair-score)))
  (define/public (pile-sum-score)
   (match (crib-cards-sum pile)
    (middle-bonus 2)
    (allowed-card-sum 1)
    (else 0)))
  (define/public (pile-run-score)
   (or (for/or ([l (in-range (length pile) (- min-run-length 1) -1)])
        (let ([p (take-right pile l)])
         (run-score p))) 0))
  (define/public (pile-pair-score)
   (define nums (map (cut send <> get-value) pile))
   (case (length (car (group (reverse nums))))
    ('2 2)
    ('3 6)
    ('4 12)
    (else 0)))
  (define/public (playable-card? card)
   (define sum (crib-cards-sum pile))
   (<= (+ sum (crib-card-value card)) allowed-card-sum))
  (define/public (card-owner card)
   (for/or ([p players] [i (in-naturals)]) (if (send p have-card? card) i #f)))
  (define/public (clear-pile)
   (set! pile '()))
  (define/public (append-pile! card)
   (set! pile (append pile (list card))))
  (define/public (select-crib player-num cards)
   (with-player-num ((pl player-num))
    (send pl remove-cards! cards)
    (set! crib (append crib cards))))
  (define/public (play-card player-num card)
   (with-player-num ((pl player-num))
    (send pl set-cards-state! (list card) 'had)
    (append-pile! card)))
  (define/public (playable? player-num)
   (with-player-num ((pl player-num))
    (send pl playable? pile)))
  (define/public (next-player current)
   (define opponent (if (= current 0) 1 0))
   (cond
    ((playable? opponent) opponent)
    ((playable? current) current)
    (else #f)))

  (define/delegate-playernum (show-card player-num table region-from region-to do-face-up))
  (define/delegate-playernum (move-player-card-to-region player-num table region))
  (define/delegate-playernum (add-score player-num dx))
  (define/delegate-playernum (get-score player-num))
  (define/delegate-playernum (all-face-up player-num table))
  ))

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
 (define nums (map (cut send <> get-value) hands))
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
 (define starter-suit (send starter get-suit))
 (define is-flush (= (length (car (group suits))) (length suits)))
 (cond
  ((and is-flush (eq? starter-suit (car suits))) 5)
  (is-flush 4)
  (else 0)))

(define (hand-score-his-nob starter cards)
 (define starter-suit (send starter get-suit))
 (if (memf (lambda (c) (eq? (send c get-suit) starter-suit)) cards)
  1 0))

(define (crib-card-value card)
 (let ([v (send card get-value)])
  (if (> v 10) 10 v)))

(define (crib-cards-sum cards)
 (for/sum ([c cards]) (crib-card-value c)))
