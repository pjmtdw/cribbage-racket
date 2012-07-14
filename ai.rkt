#lang racket

(require racket/serialize "base.rkt" "score.rkt")

(provide connect-base% ai-random% ai-normal%)

; USAGE: (combination2 for*/list ((x y) list) body)
; you can use for*, for*/list, for*/fold, ... as forstar
;-> (combination2 for*/list ((s t) '(a b c d e)) (list s t))
;'((a b) (a c) (a d) (a e) (b c) (b d) (b e) (c d) (c e) (d e))
(define-syntax combination2
 (syntax-rules ()
  ((_ forstar ((x y) ls) body ...)
   (forstar ([i (in-range (length ls))] [y (drop ls (+ 1 i))])
    (let ([x (list-ref ls i)])
     body ...)))))

(define (list-subtract s t)
 (set->list (set-subtract (list->set s) (list->set t))))

(define connect-base%
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

(define ai-base%
 (class connect-base%
  (super-new)
  (define/public (generate-crib game cards-have)
   '())
  (define/public (generate-move game cards-have)
   #f)
  (define/override (send-value v)
   (define game (get-field game (gui)))
   (define player (list-ref (get-field players game) (get-field opponent-num (gui))))
   (define cards-have (send player cards-have))
   (case (car v)
    ('select-crib #f)
    ('generate-crib 
      (for ([c (generate-crib game cards-have)])
       (super send-value `(select-crib ,(card->hash c)))))
    ('play-card #f)
    ('generate-move
     (super send-value `(play-card ,(card->hash (generate-move game cards-have)))))
    (else
     (super send-value v))))

  (define/public (start)
   (let-values ([(pin pout) (make-pipe)])
    (set-field! port-in this pin)
    (set-field! port-out this pout))
   (send this start-recv-thread))))

; play randomly
(define ai-random%
 (class ai-base%
  (super-new)
  (define/override (generate-crib game cards)
   (take (shuffle cards) cribs-per-player))
  (define/override (generate-move game cards)
   (list-ref cards (random (length cards))))))

; play card that can acquire max score on next move
(define ai-normal%
 (class ai-base%
  (super-new)
  (define/override (generate-crib game cards)
   (car
    (argmax cdr
     (combination2 for*/list ([s t] cards)
      (let [(cds (list-subtract cards (list s t)))]
       (cons (list s t) (for/sum ([s (hand-score-all #f cds)]) (cdr s))))))))

  (define/override (generate-move game cards)
   (define pile (get-field pile game))
   (car
    (argmax cdr
     (for/list ([c cards])
      (cons c (for/sum ([s (pile-score-all (append pile (list c)))]) (cdr s)))))))))
