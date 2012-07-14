#lang racket
(require srfi/1 srfi/26 games/cards racket/class "score.rkt")

(provide start-game net-port net-host ai-type ai-obj gui
 card->hash hash->card cribs-per-player number-of-cribs cribs-per-player total-hands)

(define gui (make-parameter #f))

(define net-port (make-parameter 12345))
(define net-host (make-parameter "localhost"))
(define ai-obj (make-parameter #f))
(define ai-type (make-parameter 'normal))

(define cards-per-player 6)
(define cribs-per-player 2)
(define number-of-players 2)
(define number-of-cribs (* number-of-players cribs-per-player))
(define hands-per-player (- cards-per-player cribs-per-player))
(define total-hands (* number-of-players hands-per-player))


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

(define (start-game [score1 0] [score2 0])
 (define cards (shuffle-list (make-deck) 7)); according to doc of games/cards, 7 is sufficient for shuffling list
 (make-cards-hash cards)
 (with-split-list cards ([starters 1]
                         [cards1 cards-per-player]
                         [cards2 cards-per-player])
  (let ([pls (map (match-lambda ((list c s) (make-object player% s (map (cut cons <> 'have) c))))
              (list (list cards1 score1) (list cards2 score2)))])
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
  (define/public (clear-game table)
   (send table remove-cards pile)
   (set! pile '())
   (send table remove-card starter)
   (set! starter #f)
   (send table remove-cards crib)
   (set! crib '())
   (for ([p players])
    (send table remove-cards (map car (get-field cards p)))
    (set-field! cards p '())))

  (define/public (hand-score player-num)
   (define cards
    (if player-num
     (with-player-num ((pl player-num))
      (map car (get-field cards pl)))
     crib))
   (hand-score-all starter cards))
  (define/public (pile-score)
   (pile-score-all pile))
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
  (define/delegate-playernum (all-face-up player-num table))))

