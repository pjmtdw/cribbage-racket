#lang racket

; Six Card Cribbage

(require games/cards racket/gui "base.rkt" "net.rkt" "gui.rkt" "ai.rkt")

; the instance of crib-gui must be created on early stage to avoid finishing this script before connection accomplished
(gui (new crib-gui))
(send (get-field table (gui)) show #t)

(define-syntax println
 (syntax-rules ()
  ((_ body ...)
  (displayln (format body ...)))))

(command-line
#:once-each
[("-c" "--client") "client mode" (ai-type 'client)]
[("-s" "--server") "server mode" (ai-type 'server)]
[("-a" "--ai") ai "ai type [ random | normal ]" (ai-type (string->symbol ai))]
[("-p" "--port") port "listen or connect port" (net-port (string->number port))]
[("--host") host "server hostname" (net-host host)])

(ai-obj
 (case (ai-type)
  ('server (new net-server% [port (net-port)]))
  ('client (new net-client% [host (net-host)] [port (net-port)] ))
  ('random (new ai-random%))
  ('normal (new ai-normal%))
  (else (error (format "unknown ai type: ~a" (ai-type))))))
(case (ai-type)
 ('server
  (println "listening port:~a" (net-port)))
 ('client
  (println "connecting to ~a:~a" (net-host) (net-port)))
 (else
  (println "ai-type: ~a" (ai-type))))
(send (ai-obj) start)

(define (start-main-thread)
 (thread (lambda _ (send (gui) main))))

(case (ai-type)
 ('server
  (let ([s (current-seconds)])
   (send (ai-obj) send-value `(set-seed ,s))
   (random-seed s)
   (start-main-thread)))
 ('client
  (let ([t (current-thread)])
   (send (ai-obj) set-handler 'set-seed
    (lambda (s)
     (random-seed s)
     (thread-send t 'seed-set-done)
     ))
   (thread-receive)
   (start-main-thread)))
 (else
  (start-main-thread)))

