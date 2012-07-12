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
  (region-field region-myown (200 (* 3.1 c-height) (* 4 c-width) c-height "myown cards"))
  (region-field region-opponent (200 (* 0.4 c-height) (* 4 c-width) c-height "opponent cards"))
  (region-field region-ground (150 (* 1.75 c-height) (+ 100 (* 4 c-width)) c-height "field"))
  (region-field region-starter (40 (region-y region-ground) c-width c-height "starter"))
  (region-field region-mycrib (30 (* 3.1 c-height) (* 2 c-width) c-height "my crib cards"))
  (region-field region-opcrib (30 (* 0.4 c-height) (* 2 c-width) c-height "opponent crib cards"))
  (field [message-snip #f])
  (field [game #f])
  (define/public (show-message message)
   (when message-snip (send pasteboard delete message-snip))
   (when message
    (set! message-snip (make-object string-snip% message))
    (send pasteboard insert message-snip 40 400 )))
  (define/public (main) 
   ; (start-game) must be called after (random-seed) function since both server and client have to use same seed.
   (set-field! game this (start-game))
   (define-values (player-num opponent-num)
    (case (net-mode)
     ('server
      (values 0 1))
     ('client
      (values 1 0))))
   (send game show-player-card table region-myown player-num)
   (send game show-player-card table region-opponent opponent-num)
   (show-message "please choose two crib cards")
   (send table set-single-click-action (lambda (card) (send net-obj send-value (list 'seed (number->string (current-seconds)))))))))
