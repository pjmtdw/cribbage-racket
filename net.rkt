#lang racket

(require racket/tcp "ai.rkt")

(provide net-client% net-server% net-sync-value)

(define (ai-obj->symbol obj)
 (if (is-a? obj net-server%) 'server 'client))

(define net-client%
 (class connect-base%
  (super-new)
  (init-field port)
  (init-field host)
  (define/public (start)
   (let loop ()
    (let-values ([(in out)
                  (with-handlers ([exn:fail:network? (lambda(e) (values #f #f)) ]) (tcp-connect host port))
                 ])
     (if in
      (begin
       (set-field! port-in this in)
       (set-field! port-out this out))
      (begin
       (sleep 1)
       (loop)))))
   (send this start-recv-thread))))

(define net-server%
 (class connect-base%
  (super-new)
  (init-field port)
  (define/public (start)
   (define listener (tcp-listen port))
   (let-values ([(in out) (tcp-accept listener)])
    (set-field! port-in this in)
    (set-field! port-out this out))
   (send this start-recv-thread))))

; usage (net-sync-value ai-obj 'server value)
(define-syntax net-sync-value
 (syntax-rules ()
  ((_ ai-obj server-symbol expr)
   (if (eq? (ai-obj->symbol ai-obj) server-symbol)
    (let ([v expr])
     (send ai-obj send-value v)
     v)
    (send ai-obj recv-value)))))

