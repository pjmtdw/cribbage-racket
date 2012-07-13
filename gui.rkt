#lang racket
(require games/cards racket/gui "base.rkt" srfi/26)
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

(define (make-text-snip text [editor #f] [color #f] [size-add #f])
 (let*
  ([snip (make-object string-snip% text)]
   [delta (new style-delta%)]
   [style-list (if editor (send editor get-style-list) #f)])
  (when color
   (send delta set-delta-foreground color))
  (when size-add
   (send delta set-size-add size-add))
  (when (and editor (or color size-add))
    (let ([new-style (send style-list find-or-create-style (send snip get-style) delta)])
     (send snip set-style new-style)))
  snip))


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
  (field [game #f])
  (field [player-num #f] [opponent-num #f] [dealer-num #f] [child-num #f])
  (field [player-color #f] [opponent-color #f])

  (define/show-text-snip (show-message message) message-snip 40 400)
  (define/show-text-snip (show-opponent-score score) opponent-score-snip 500 (+ 25 (region-y region-opponent)) pasteboard player-color 7)
  (define/show-text-snip (show-player-score score) player-score-snip 500 (+ 25 (region-y region-myown)) pasteboard opponent-color 7)
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

  (define (show-scores)
   (show-opponent-score (send game get-score opponent-num))
   (show-player-score (send game get-score player-num)))

  (define (print-score num str . arg)
   (define pl (if (= num player-num) "[You]" "[Opponent]"))
   (apply printf (cons (string-append pl " " str "\n") arg))
   (flush-output))

  (define/public (phase-the-play)
   (send game move-player-card-to-region opponent-num table region-opponent)
   (send game move-player-card-to-region player-num table region-myown)
   (show-message "now playing")
   (define nums-in-ground 0)
   (define current-player child-num) 
   (define (show-whose-turn)
    (when (and current-player player-num)
     (if (= current-player player-num)
      (show-message "your turn")
      (show-message "opponent's turn"))))

   ;Two for his heels
   (when (= 11 (send (get-field starter game) get-value))
    (send game add-score dealer-num 2)
    (print-score dealer-num "Two for his heels: +2")
    (show-scores))

   (show-whose-turn)
   (define (move-to-ground card)
    (send table card-face-up card)
    (move-card-aligned card region-ground total-hands nums-in-ground)
    (set! nums-in-ground (+ 1 nums-in-ground)))
   (click-action-and-handler 'play-card card
    ([and (< nums-in-ground total-hands) (own-card? card) (= current-player player-num) (send game playable-card? card) ]
      (move-to-ground card)
      (send game play-card player-num card)
      )
    ((move-to-ground card)
     (send game play-card opponent-num card)
    )
    )
   (let loop ()
    (unless (>= nums-in-ground total-hands)
      (thread-receive)
      (let* ([score (send game pile-score)]
             [sum (for/sum ([i score]) (cdr i))])
       (send game add-score current-player sum)
       (when (> sum 0) (print-score current-player "Score: +~a" (filter (lambda (x) (> (cdr x) 0)) score))))
      (let ([nextp (send game next-player current-player)])
       (if nextp (set! current-player nextp)
        (begin 
         (send table cards-face-down (get-field pile game))
         ;Last Card Score
         (send game add-score current-player 1)
         (print-score current-player "Last Card: +1")
         (send game clear-pile)
         (set! current-player (send game next-player current-player)))))
      (show-scores)
      (show-whose-turn)
      (loop)
      ))
   (send table set-single-click-action identity))
  (define/public (phase-the-show)
   (define curth (current-thread))
   (send game move-player-card-to-region opponent-num table region-opponent)
   (send game move-player-card-to-region player-num table region-myown)
   (send game all-face-up opponent-num table)
   (send game all-face-up player-num table)
   (send table cards-face-up (get-field crib game))
   (define received '())
   (define (do-receive s c)
    (define (check-cond)
     (= c (count (cut eq? s <>) received)))
    (unless (check-cond)
     (let loop ()
       (set! received (cons (thread-receive) received))
       (unless (check-cond) 
        (loop)))))

   (send (net-obj) add-handler 'next-game
    (lambda _
     (thread-send curth 'next-game)))
   (define button-region (make-button-region 500 400 50 25 "next" (lambda _ (thread-send curth 'next))))
   (send table add-region button-region)
   (define (doit s c num name)
    (let* ([score (send game hand-score num)]
           [filterd (filter (lambda (x) (> (cdr x) 0)) score)]
           [sum (for/sum ([c score]) (cdr c))]
           [pnum (if num num dealer-num)])
     (send game add-score pnum sum)
     (print-score pnum "The Show: +~a" filterd)
     (show-message (format (string-append name ":~a") filterd))
     (show-scores)
     (do-receive s c)))
   (doit 'next 1 child-num "child")
   (doit 'next 2 dealer-num "dealer")
   (doit 'next 3 #f "crib")
   (send table remove-region button-region)
   (send (net-obj) send-value '(next-game))
   (do-receive 'next-game 1)
   )

  (define/public (main)
   ; (start-game) must be called after (random-seed) function since both server and client have to use same seed.
   (set-field! game this (start-game))
   (set-values-by-net-mode! (player-num opponent-num) (0 1))
   (set-values-by-net-mode! (player-color opponent-color) ("orange" "purple"))
   (set! dealer-num 0)
   (set! child-num 1)
   (send table add-cards-to-region (list (get-field starter game)) region-starter)
   (send game show-card opponent-num table region-starter region-opponent #f)
   (send game show-card player-num table region-starter region-myown #t)
   (show-opponent-score 0)
   (show-player-score 0)
   (phase-choose-crib)
   (phase-the-play)
   (phase-the-show)
   (displayln "TODO: Go For Next Game")
   )))

(define-syntax set-values-by-net-mode!
 (syntax-rules ()
  ((_ (p-k o-k) (p-v o-v))
   (let-values ([(p o)
    (case (net-mode)
     ('server
      (values p-v o-v))
     ('client
      (values o-v p-v)))])
    (set! p-k p)
    (set! o-k o)))))

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

(define-syntax define/show-text-snip
 (syntax-rules ()
  ((_ (func arg) snip x y rest ...)
   (begin
    (field [snip #f])
    (define/public (func arg)
     (let ([pasteboard (get-field pasteboard this)]
           [text (if (number? arg) (number->string arg) arg)])
      (when snip (send pasteboard delete snip))
      (when text
       (set! snip (make-text-snip text rest ...))
       (send pasteboard insert snip x y))))))))
