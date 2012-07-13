#lang racket

(require racket/tcp racket/serialize)

(provide net-client% net-server% net-sync-value)

(define (net-obj->symbol obj)
 (if (is-a? obj net-server%) 'server 'client))

(define net-base%
 (class object%
  (super-new)
  (init-field [sock-in #f])
  (init-field [sock-out #f])
  (field [handlers (make-hasheq)])
  (define/public (send-value v)
   (write (serialize v) sock-out)
   (flush-output sock-out))
  (define/public (recv-value)
   (deserialize (read sock-in)))
  (define/public (set-handler key func)
   (hash-set! handlers key func))
  (define/public (remove-handler key)
   (hash-remove handlers key))
  (define/public (recv-thread)
   (let loop ()
    (let* ([v (deserialize (read sock-in))]
           [func (hash-ref handlers (car v))]
           )
     (apply func (cdr v))
     (loop))))))

(define net-client%
 (class net-base%
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
       (set-field! sock-in this in)
       (set-field! sock-out this out))
      (begin
       (sleep 1)
       (loop)))))
   (thread (lambda () (send this recv-thread))))))

(define net-server%
 (class net-base%
  (super-new)
  (init-field port)
  (define/public (start)
   (define listener (tcp-listen port))
   (let-values ([(in out) (tcp-accept listener)])
    (set-field! sock-in this in)
    (set-field! sock-out this out))
   (thread (lambda () (send this recv-thread))))))

; usage (net-sync-value net-obj 'server value)
(define-syntax net-sync-value
 (syntax-rules ()
  ((_ net-obj server-symbol expr)
   (if (eq? (net-obj->symbol net-obj) server-symbol)
    (let ([v expr])
     (send net-obj send-value v)
     v)
    (send net-obj recv-value)))))

