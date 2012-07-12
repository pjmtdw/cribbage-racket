#lang racket
(require games/cards racket/gui "base.rkt")

(provide game-main)

(define (game-main) 
 (define-values (c-width c-height)
  (let ([c (car (make-deck))])
   (values
    (send c card-width)
    (send c card-height))))

 (define region-myown (make-region 200 (* 3.1 c-height) (* 4 c-width) c-height "myown cards" #f))
 (define region-opponent (make-region 200 (* 0.4 c-height) (* 4 c-width) c-height "opponent cards" #f))
 (define region-field (make-region 150 (* 1.75 c-height) (+ 100 (* 4 c-width)) c-height "field" #f))
 (define region-starter (make-region 40 (region-y region-field) c-width c-height "starter" #f))
 (define region-mycrib (make-region 30 (* 3.1 c-height) (* 2 c-width) c-height "my crib cards" #f))
 (define region-opcrib (make-region 30 (* 0.4 c-height) (* 2 c-width) c-height "opponent crib cards" #f))

 (send (table) add-region region-myown)
 (send (table) add-region region-opponent)
 (send (table) add-region region-field)
 (send (table) add-region region-starter)
 (send (table) add-region region-mycrib)
 (send (table) add-region region-opcrib)
 (define canvas (car (send (table) get-children)))
 (define pasteboard (send canvas get-editor))

 (define message-snip #f)
 (define (show-message message)
  (when message-snip (send pasteboard delete message-snip))
  (when message
   (set! message-snip (make-object string-snip% message))
   (send pasteboard insert message-snip 40 400 )))
 (define game (start-game))
 (define-values (player-num opponent-num)
  (case (net-mode)
   ('server
    (values 0 1))
   ('client
    (values 1 0))))
 (send game show-player-card (table) region-myown player-num)
 (send game show-player-card (table) region-opponent opponent-num)
 (show-message "please choose two crib cards")
 (send (table) set-single-click-action (lambda (card) (send net-obj send-value (list 'seed (number->string (current-seconds)))))))
