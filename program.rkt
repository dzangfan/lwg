#lang racket/base

(require "exn.rkt")
(require "normalize.rkt")
(require "runtime.rkt")
(require racket/path)
(require racket/cmdline)
(require racket/match)

(struct lwg-args (source-path handler-names+ graph-argv)
  #:transparent)

(define (parse-command-line [argv (current-command-line-arguments)] #:port [port (current-output-port)])
  (define handler-names+ null)
  (parameterize ([current-output-port port])
    (command-line #:argv argv
                  #:multi
                  [("-H" "--handler") handler "Activate a handler"
                                      (set! handler-names+
                                            (cons (string->symbol handler)
                                                  handler-names+))]
                  #:args (filename . graph-argv)
                  (lwg-args (normalize-path filename)
                            handler-names+
                            graph-argv))))

(define (evaluate-assignment assignment runtime)
  (define new-value
    (match (assignment-value assignment)
      [(list 'SINGLE-STR value) value]
      [(list 'DOUBLE-STR value) value]
      [(list 'REF path)
       (get-runtime-variable runtime path)]
      [value
       (raise (exn:fail:lwg-runtime-error (format "Unknown right-value: ~A"
                                                  value))
              (current-continuation-marks))]))
  (make-assignment (assignment-path assignment)
                   new-value
                   #:from (assignment-start-pos assignment)
                   #:to (assignment-end-pos assignment)))

(module+ test
  (require rackunit)
  (test-case "Command line arguments test"
    (check-equal? (parse-command-line '("a.lwg" "x" "y" "z"))
                  (lwg-args (normalize-path "a.lwg")
                            null
                            '("x" "y" "z")))
    (check-equal? (parse-command-line '("-H" "foo" "--handler" "bar" "b.lwg"))
                  (lwg-args (normalize-path "b.lwg")
                            '(bar foo)
                            null))))

(module+ main
  (require "syntax.rkt")
  (require "normalize.rkt")
  (require "runtime.rkt")
  (require "lwg-runtime.rkt")
  (require "handlers/main.rkt")
  (require racket/list)
  (require racket/match)
  (require racket/format)
  (define stdout (current-output-port))
  (define old-stdout-collector (open-output-string))
  (parameterize ([current-output-port old-stdout-collector])
    (lwg-open-receiver 'debug stdout)

    (define builtin-handler-names
      '(auto-store))
    
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (lwg-log/plain 'lwg 'fatal (exn-message exn)))])
      (define args (parse-command-line #:port stdout))
      (define runtime (empty-runtime))
      (for ([handler-name builtin-handler-names])
        (when (add-handler! runtime (require-handler handler-name))
          (lwg-log 'lwg 'info "Built-in handler ~A has been activated"
                   handler-name)))
      (for ([handler-name (lwg-args-handler-names+ args)])
        (when (add-handler! runtime (require-handler handler-name))
          (lwg-log 'lwg 'info "Handler ~A has been activated." handler-name)))

      (lwg-log 'lwg 'info "~A handlers has been activated."
               (length (runtime-handlers runtime)))
      (define source-path (lwg-args-source-path args))
      (define source-path/str (path->string source-path))
      (lwg-log 'lwg 'info "Source: ~A" source-path/str)
      (define source-port (open-input-file source-path))
      (define ast (read-AST source-port))
      (define assignment-list (normalize ast))
      (lwg-log 'lwg 'info "Compilation completes.")
      (define 1st-a (first assignment-list))
      (define last-a (last assignment-list))
      (define argv-assignment-list
        (for/list ([i (in-naturals)]
                   [argv (lwg-args-graph-argv args)])
          (lwg-log 'lwg 'debug "argv.~A: ~A" i argv)
          (make-assignment (list "argv" (~a i))
                           (list 'SINGLE-STR argv)
                           #:from (assignment-start-pos 1st-a)
                           #:to (assignment-end-pos last-a))))
      (define assignment-list+
        (append argv-assignment-list
                (list (make-assignment '("__FILE__" "path")
                                       (list 'SINGLE-STR source-path/str)
                                       #:from (assignment-start-pos 1st-a)
                                       #:to (assignment-end-pos 1st-a))
                      (make-assignment '("__FILE__" "start")
                                       #:from (assignment-start-pos 1st-a)
                                       #:to (assignment-end-pos 1st-a)))
                assignment-list
                (list (make-assignment '("__FILE__" "end")
                                       #:from (assignment-start-pos last-a)
                                       #:to (assignment-end-pos last-a)))))

      (define generated-assignment-list null)

      (define-runtime-service #:gen-assignment (handler-name runtime assignment)
        (lambda (path #:start-pos [start-pos (assignment-start-pos assignment)]
                      #:end-pos [end-pos (assignment-end-pos assignment)]
                      #:= [value default-value])
          (define assignment
            (make-assignment path value #:from start-pos #:to end-pos))
          (set! generated-assignment-list
                (append generated-assignment-list (list assignment)))))

      (let handle-all ([current-assignment-list assignment-list+])
        (unless (null? current-assignment-list)
          (define assignment (first current-assignment-list))
          (define assignment+ (evaluate-assignment assignment runtime))
          (handle runtime assignment+ #:collect-result #f)
          (cond [(null? generated-assignment-list)
                 (handle-all (rest current-assignment-list))]
                [else (define next-assignment-list
                        (append generated-assignment-list
                                (rest current-assignment-list)))
                      (set! generated-assignment-list null)
                      (handle-all next-assignment-list)])))
      
      (lwg-log 'lwg 'info "Execution completes."))

    (lwg-close-receivers)))
