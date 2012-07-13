#lang racket
(require games/cards racket/gui "base.rkt")
(provide crib-gui)

(define-syntax region-field
 (syntax-rules ()
  ((_ r (body ...))
   (field [r (let ([x (make-region body ... #f)]) (send (get-field table this) add-region x) x)]))))


(define-values (c-width c-height)
 (let ([c (car (make-deck))])
  (values
   (send c card-width)
   (send c card-height))))

(define crib-gui
 (class object%
  (super-new)
  (field [table (make-table "Cribbage" 8 4.5)])
  (field [canvas (car (send table get-children))])
  (field [pasteboard (send canvas get-editor)])
  (define crib-width (* 2 c-width))
  (define cards-width (* 4 c-width))
  (region-field region-myown (200 (* 3.1 c-height) cards-width c-height "myown cards"))
  (region-field region-opponent (200 (* 0.4 c-height) cards-width c-height "opponent cards"))
  (region-field region-ground (150 (* 1.75 c-height) (+ 100 cards-width) c-height "field"))
  (region-field region-starter (40 (region-y region-ground) c-width c-height "starter"))
  (region-field region-mycrib (30 (* 3.1 c-height) crib-width c-height "my crib cards"))
  (region-field region-opcrib (30 (* 0.4 c-height) crib-width c-height "opponent crib cards"))
  (field [message-snip #f])
  (field [game #f])
  (field [player-num #f] [opponent-num #f] [dealer-num #f])
  (define/public (show-message message)
   (when message-snip (send pasteboard delete message-snip))
   (when message
    (set! message-snip (make-object string-snip% message))
    (send pasteboard insert message-snip 40 400 )))
  (define/public (move-card-aligned card region maxnum index)
   (define x (+ (region-x region) (* index (/ (- (region-w region) c-width) (- maxnum 1)))))
   (define y (region-y region))
   (send table card-to-front card)
   (send table move-card card  x y))
  (define (own-card? card)
   (= player-num (send game card-owner card)))
  (define/public (phase-choose-crib)
   (show-message "please choose two crib cards")
   (define (move-to-crib card)
    (define reg
     (if (= dealer-num player-num) region-mycrib region-opcrib))
    (send table card-face-down card)
    (move-card-aligned card reg number-of-cribs (length (get-field crib game))))
   (define selected-crib 0)
   (click-action-and-handler 'select-crib card
    ([and (< selected-crib cribs-per-player) (own-card? card)]
     (move-to-crib card)
     (send game select-crib player-num (list card))
     (set! selected-crib (+ selected-crib 1)))
    ((move-to-crib card)
     (send game select-crib opponent-num (list card))))
   (let loop ()
    (unless (>= (length (get-field crib game)) number-of-cribs)
      (thread-receive)
      (loop)
      ))
   (send table card-face-up (get-field starter game)))

  (define/public (phase-play-card)
   (send game realign-card opponent-num table region-opponent)
   (send game realign-card player-num table region-myown)
   (show-message "now playing")
   (define nums-in-ground 0)
   (define (move-to-ground card)
    (send table card-face-up card)
    (move-card-aligned card region-ground total-hands nums-in-ground)
    (set! nums-in-ground (+ 1 nums-in-ground)))
   (click-action-and-handler 'play-card card
    ([and (< nums-in-ground total-hands) (own-card? card)]
      (move-to-ground card)
         ;TODO: (send game play-card ...)
      )
    ((move-to-ground card)
     ;TODO: (send game play-card ...)
    )
    )
   (let loop ()
    (unless (>= nums-in-ground total-hands)
      (thread-receive)
      (loop)
      )))

  (define/public (main)
   ; (start-game) must be called after (random-seed) function since both server and client have to use same seed.
   (set-field! game this (start-game))
   (let-values ([(p-num o-num)
    (case (net-mode)
     ('server
      (values 0 1))
     ('client
      (values 1 0)))])
    (set! player-num p-num)
    (set! opponent-num o-num))
   (set! dealer-num 0)
   (send table add-cards-to-region (list (get-field starter game)) region-starter)
   (send game show-card opponent-num table region-starter region-opponent #f)
   (send game show-card player-num table region-starter region-myown #t)
   (phase-choose-crib)
   (phase-play-card)
   )))

(define-syntax click-action-and-handler
 (syntax-rules ()
  ((_ sym card (condition action ...) ( handler ...))
   (begin
    (let ([curth (current-thread)])
     (send (get-field table this) set-single-click-action
      (lambda (card)
       (when condition
        action ...
        (send (net-obj) send-value (list sym (card->hash card)))
        (thread-send curth sym))))
     (send (net-obj) add-handler sym
      (lambda (hash)
       (let ([card (hash->card hash)])
        handler ...
        (thread-send curth sym)))))))))
