#lang racket

; Six Card Cribbage

(require games/cards racket/gui "base.rkt" "net.rkt" "game.rkt")

(table (make-table "Cribbage" 8 4.5))
(send (table) show #t)

(define-syntax println
 (syntax-rules ()
  ((_ body ...)
  (displayln (format body ...)))))

(command-line
#:once-each
[("-c" "--client") "client mode" (net-mode 'client)]
[("-s" "--server") "server mode" (net-mode 'server)]
[("-p" "--port") port "listen or connect port" (net-port (string->number port))]
[("--host") host "server hostname" (net-host host)])

(net-obj 
 (case (net-mode)
  ('server (new net-server% [port (net-port)]))
  ('client (new net-client% [host (net-host)] [port (net-port)] ))
  (else (error "you must supply either -s or -c"))))
(case (net-mode)
 ('server
  (println "listening port:~a" (net-port)))
 ('client
  (println "connecting to ~a:~a" (net-host) (net-port))))
(send (net-obj) start)
(displayln "connected")

(case (net-mode)
 ('server
  (let ([s (current-seconds)])
   (displayln "sending seed")
   (send (net-obj) send-value `(set-seed ,s))
   (random-seed s)
   (game-main)))
 ('client
  (send (net-obj) add-handler 'set-seed
   (lambda (s)
    (random-seed s)
    (game-main)))))

