#lang racket/base

(require "exn.rkt")
(require "runtime.rkt")
(require racket/contract)
(require racket/match)
(require racket/string)
(require racket/logging)

(define lwg-handlers (make-hasheq))

(define (provide-handler handler)
  (hash-set! lwg-handlers (handler-name handler) handler))

(define (require-handler handler-name)
  (match (hash-ref lwg-handlers handler-name #f)
    [#f (raise (exn:fail:lwg-runtime-error
                (format "Unknown handler '~A'" handler-name)
                (current-continuation-marks)))]
    [handler handler]))

(provide
 (contract-out [provide-handler (-> handler? any/c)]
               [require-handler (-> symbol? handler?)]))

(define-logger lwg #:parent #f)

(define (lwg-log/plain topic level message)
  (log-message lwg-logger level topic message #f #f))

(define (lwg-log* topic level format-string args)
  (define message (apply format format-string args))
  (lwg-log/plain topic level message))

(define (lwg-log topic level format-string . args)
  (lwg-log* topic level format-string args))

(define (prettify-string string)
  (let* ([normalized#0 (string-replace string #px"\r\n|\r" "\n")]
         [normalized (string-trim normalized#0)]
         [lines (string-split normalized "\n")])
    (match (length lines)
      [0 "<empty>"]
      [1 normalized]
      [_ (define indented
           (map (lambda (line)
                  (string-append "\n  " (string-trim line #:left? #f)))
                lines))
         (string-append* indented)])))

(define lwg-receiver-threads null)

(define (lwg-open-receiver level port)
  (define receiver (make-log-receiver lwg-logger level))
  (define prefix-table
    #hasheq((debug . "(D)")
            (info . "[I]")
            (warning . "[W]")
            (error . "<E>")
            (fatal . "<!>")))
  (define receiver-thd
    (thread
     (lambda ()
       (let continue ()
         (match-let ([(vector level msg break? topic) (sync receiver)])
           (let ([prefix (hash-ref prefix-table level "(?)")])
             (fprintf port "~A ~A: ~A~%" prefix topic (prettify-string msg)))
           (unless break? (continue)))))))
  (set! lwg-receiver-threads
        (cons receiver-thd lwg-receiver-threads)))

(define (lwg-close-receivers)
  (log-message lwg-logger 'info 'lwg "Quit." #t #f)
  (for ([thd lwg-receiver-threads])
    (thread-wait thd))
  (set! lwg-receiver-threads null))

(define-runtime-service #:logger/debug (handler-name runtime assignment)
  (lambda (format-string . args)
    (lwg-log* handler-name 'debug format-string args)))

(define-runtime-service #:logger/info (handler-name runtime assignment)
  (lambda (format-string . args)
    (lwg-log* handler-name 'info format-string args)))

(define-runtime-service #:logger/warning (handler-name runtime assignment)
  (lambda (format-string . args)
    (lwg-log* handler-name 'warning format-string args)))

(define-runtime-service #:logger/error (handler-name runtime assignment)
  (lambda (format-string . args)
    (lwg-log* handler-name 'error format-string args)))

(define-runtime-service #:logger/fatal (handler-name runtime assignment)
  (lambda (format-string . args)
    (lwg-log* handler-name 'fatal format-string args)))

(provide
 (contract-out [lwg-log/plain (-> symbol? log-level/c string? any/c)]
               [lwg-log* (-> symbol? log-level/c string? list? any/c)]
               [lwg-log (-> symbol? log-level/c string? any/c ... any/c)]
               [lwg-open-receiver (-> log-level/c output-port? any/c)]
               [lwg-close-receivers (-> any/c)]))

(module+ test
  (require rackunit)
  (require "normalize.rkt")
  (test-case "Global handler test"
    (define-handler h)
    (provide-handler h)
    (define my-h (require-handler 'h))
    (check-eq? (handler-name my-h) 'h)
    (check-exn exn:fail:lwg-runtime-error? (lambda () (require-handler 'm))))
  (test-case "Primitive logger test"
    (define output (open-output-string))
    (lwg-open-receiver 'debug output)
    (lwg-log 'joke 'debug "Hello ~A!" "Joe")
    (lwg-log 'joke 'info "How are you?")
    (lwg-log 'serious 'warning "Me philosophia do\r\n\n  -- Seneca")
    (lwg-log 'joke 'error "Fine.\n")
    (lwg-log 'joke 'fatal "\n  Bye")
    (lwg-log 'joke 'info "...")
    (lwg-close-receivers)
    (define log-result
#<<EOF
(D) joke: Hello Joe!
[I] joke: How are you?
[W] serious: 
  Me philosophia do
  
    -- Seneca
<E> joke: Fine.
<!> joke: Bye
[I] joke: ...
[I] lwg: Quit.

EOF
      )
    (check-equal? (get-output-string output)
                  log-result))
  (test-case "Handlers' logging test -- cases"
    (define-handler debug-handler
      #:use [#:logger/debug debug]
      #:use [#:logger/info info]
      #:use [#:logger/warning warn]
      #:use [#:logger/error err]
      #:use [#:logger/fatal fatal]
      #:case
      [("Alice" "say" = content)
       (info "~A -- Alice" content)]
      #:case
      [("Bob" "say" = content)
       (err "~A -- Bob" content)]
      #:case
      [("Carol" "say" = content)
       (warn "~A -- Carol" content)]
      #:case
      [(name "say" = content)
       (debug "~A -- ~A" content name)
       (fatal "Hello, ~A! -- others" name)])
    (define assignment-list
      (list (make-assignment '("Carol" "say") "Hello!")
            (make-assignment '("Alice" "say") "Hi.")
            (make-assignment '("Bob" "say") "...")
            (make-assignment '("David" "say") "may i come in?")))
    (define runtime (empty-runtime))
    (add-handler! runtime debug-handler)
    (define output (open-output-string))
    (lwg-open-receiver 'debug output)
    (for ([assignment assignment-list])
      (handle runtime assignment))
    (lwg-close-receivers)
    (check-equal? (get-output-string output)
                  #<<EOF
[W] debug-handler: Hello! -- Carol
[I] debug-handler: Hi. -- Alice
<E> debug-handler: ... -- Bob
(D) debug-handler: may i come in? -- David
<!> debug-handler: Hello, David! -- others
[I] lwg: Quit.

EOF
                  ))
  (test-case "Handlers' logging test -- initially/finally"
    (define-handler debug-handler
      #:use [#:logger/debug debug]
      #:use [#:logger/info info]
      #:use [#:logger/warning warn]
      #:use [#:logger/error err]
      #:use [#:logger/fatal fatal]
      #:initially (debug "I")
      #:initially (info "I")
      #:initially (warn "I")
      #:initially (err "I")
      #:initially (fatal "I")
      #:finally (debug "F")
      #:finally (info "F")
      #:finally (warn "F")
      #:finally (err "F")
      #:finally (fatal "F"))
    (define runtime (empty-runtime))
    (add-handler! runtime debug-handler)
    (define output (open-output-string))
    (lwg-open-receiver 'debug output)
    (handle runtime (make-assignment '("a") "b"))
    (lwg-close-receivers)
    (check-equal? (get-output-string output)
                  #<<EOF
(D) debug-handler: I
[I] debug-handler: I
[W] debug-handler: I
<E> debug-handler: I
<!> debug-handler: I
(D) debug-handler: F
[I] debug-handler: F
[W] debug-handler: F
<E> debug-handler: F
<!> debug-handler: F
[I] lwg: Quit.

EOF
                  )))
