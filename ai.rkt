#lang racket

(require racket/serialize "base.rkt")

(provide ai-base% ai-random%)

(define ai-base%
 (class object%
  (super-new)
  (init-field [port-in #f])
  (init-field [port-out #f])
  (field [handlers (make-hasheq)])
  (define/public (send-value v)
   (write (serialize v) port-out)
   (flush-output port-out))
  (define/public (recv-value)
   (deserialize (read port-in)))
  (define/public (set-handler key func)
   (hash-set! handlers key func))
  (define/public (remove-handler key)
   (hash-remove handlers key))
  (define/public (apply-handler sym args)
   (define func (hash-ref handlers sym))
   (apply func args))
  (define/public (start-recv-thread)
   (thread
    (lambda ()
     (let loop ()
      (let* ([v (deserialize (read port-in))])
       (apply-handler (car v) (cdr v))
       (loop))))))))

(define ai-random%
 (class ai-base%
  (super-new)
  (define/override (send-value v)
   (define game (get-field game (gui)))
   (define player (list-ref (get-field players game) (get-field opponent-num (gui))))
   (define cards-have (send player cards-have))
   (case (car v)
    ('select-crib #f)
    ('generate-crib
      (for ([c (take (shuffle cards-have) 2)])
       (super send-value `(select-crib ,(card->hash c)))))
    ('play-card #f)
    ('generate-move
     (super send-value `(play-card ,(card->hash (list-ref cards-have (random (length cards-have)))))))
    (else
     (super send-value v))))

  (define/public (start)
   (let-values ([(pin pout) (make-pipe)])
    (set-field! port-in this pin)
    (set-field! port-out this pout))
   (send this start-recv-thread))))
