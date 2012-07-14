#lang racket

(require racket/generator racket/serialize srfi/26 "base.rkt" "score.rkt")

(provide connect-base% ai-random% ai-normal%)

(define (in-combination ls depth)
 (in-generator
  (let go ([d depth] [l ls] [r '()])
   (if (= d 0) (yield (reverse r))
    (for ([x l] [i (in-naturals)])
     (go (- d 1) (drop l (+ i 1)) (cons x r)))))))

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
  ; you must override generate-crib and generate-move in the derived classes
  ; TODO: is there any way to force it using something like Interface ?
  (define/public (generate-crib game cards-have)
   '())
  (define/public (generate-move game cards-have)
   #f)
  (define/override (send-value v)
   (define game (get-field game (gui)))
   (define player (list-ref (get-field players game) (get-field opponent-num (gui))))
   (define cards-have (filter (cut send game playable-card? <>) (send player cards-have)))
   (case (car v)
    ('select-crib #f)
    ('generate-crib
      (for ([c (generate-crib game cards-have)])
       (super send-value `(select-crib ,(card->hash c)))))
    ('play-card #f)
    ('generate-move
     (let ([c (generate-move game cards-have)])
      (unless (send game playable-card? c)
       (error "AI generated illegal move"))
      (super send-value `(play-card ,(card->hash c)))))
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
     (for/list ([cs (in-combination cards 2)])
      (let ([cds (list-subtract cards cs)])
       (cons cs (for/sum ([s (hand-score-all #f cds)]) (cdr s))))))))

  (define/override (generate-move game cards)
   (define pile (get-field pile game))
   (car
    (argmax cdr
     (for/list ([c cards])
      (cons c (for/sum ([s (pile-score-all (append pile (list c)))]) (cdr s)))))))))
