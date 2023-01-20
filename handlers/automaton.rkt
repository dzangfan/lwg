#lang racket/base

(require "../exn.rkt")
(require "utils.rkt")
(require "../runtime.rkt")
(require "../lwg-runtime.rkt")
(require racket/list)
(require racket/string)

(struct automaton*
  (start accept-list state-list edge-list state-attr-table)
  #:constructor-name make-automaton
  #:transparent
  #:mutable)

(define (add-accept! automaton state-name)
  (set-automaton*-accept-list!
   automaton
   (pushnew state-name (automaton*-accept-list automaton))))

(define (add-state-attr! automaton state attr value)
  (define state-attr-table
    (automaton*-state-attr-table automaton))
  (define origin-attr
    (hash-ref state-attr-table state #hash()))
  (hash-set! state-attr-table state (hash-set origin-attr attr value)))

(define (add-state! automaton state-name)
  (set-automaton*-state-list!
   automaton
   (pushnew state-name (automaton*-state-list automaton))))

(define (add-edge! automaton from-state to-state)
  (set-automaton*-edge-list!
   automaton
   (pushnew (list from-state to-state)
            (automaton*-edge-list automaton))))

(define (write-automaton->graphviz automaton port var)
  (displayln "digraph {" port)
  (displayln "  rankdir = LR;" port)
  (displayln "  node [shape = circle];" port)
  (display "  __START__" port)
  (write-hash->graphviz-attr-list
   #hash(("color" . "transparent") ("label" . "")) port)
  (displayln ";" port)

  (for ([state (automaton*-state-list automaton)])
    (fprintf port "  ~A" (string->graphviz-id state))
    (when (member state (automaton*-accept-list automaton))
      (add-state-attr! automaton state "shape" "doublecircle"))
    (define attr-table
      (hash-ref (automaton*-state-attr-table automaton) state #f))
    (write-hash->graphviz-attr-list attr-table port)
    (displayln ";" port))

  (add-edge! automaton "__START__" (automaton*-start automaton))
  (for ([edge (automaton*-edge-list automaton)])
    (define-values (from-state to-state)
      (values (first edge) (second edge)))
    (fprintf port "  ~A -> ~A"
             (string->graphviz-id from-state)
             (string->graphviz-id to-state))
    (define input (var "move" from-state to-state))
    (unless (or (string=? "" input) (string=? "__START__" from-state))
      (write-hash->graphviz-attr-list (hash "label" input) port))
    (displayln ";" port))
  
  (display "}" port))

(define-handler automaton
  #:use [#:data automaton #:set-data! init-automaton!]
  #:use [#:logger/info info #:logger/warning warn
         #:logger/debug debug #:logger/fatal die]
  #:case
  [("__FILE__" "start")
   (init-automaton! (make-automaton #f null null null (make-hash)))]
  #:case
  (["__GRAPH__" "node" state-name]
   (add-state! automaton state-name))
  #:case
  (["__GRAPH__" "edge" from-state to-state]
   (add-edge! automaton from-state to-state))
  #:use [#:set-var! set-var!]
  #:case
  (["start" "from" state]
   (when (not (member state (automaton*-state-list automaton)))
     (warn "Implicitly define state ~A" state)
     (add-state! automaton state))
   (set-automaton*-start! automaton state)
   (info "Starts from ~A" state))
  #:case
  (["state" state attr = value]
   (unless (member state (automaton*-state-list automaton))
     (die "Encountered a undeclared state [~A]" state)
     (raise (exn:fail:lwg-runtime-error "Assign attribution for unknown state"
                                        (current-continuation-marks))))
   (add-state-attr! automaton state attr value)
   (info "* ~A [~A = ~A]" state attr (shorten value)))
  #:case
  (["move" from-state to-state = input]
   (when (and (not (string=? from-state "*"))
              (not (string=? to-state "()"))
              (not (member (list from-state to-state)
                           (automaton*-edge-list automaton))))
     (warn "Implicitly define transit ~A -> ~A" from-state to-state)
     (add-edge! automaton from-state to-state))
   (set-var! input "move" from-state to-state)
   (info "Jump from ~A to ~A by ~A" from-state to-state
         (shorten input)))
  #:case
  (["accept" state]
   (when (not (member state (automaton*-state-list automaton)))
     (warn "Implicitly define state ~A" state)
     (add-state! automaton state))
   (add-accept! automaton state)
   (info "Accepts ~A" state))
  #:use [#:var var]
  #:case
  (["__FILE__" "end"]
   (cond [(automaton*-start automaton) (void)]
         [(not (null? (automaton*-state-list automaton)))
          (warn "[start.from] is not provided, use the first state (~A) instead"
                (first (automaton*-state-list automaton)))
          (set-automaton*-start! automaton
                                 (first (automaton*-state-list automaton)))]
         [else (die "Cannot build an automaton without state")
               (raise (exn:fail:lwg-runtime-error "Empty input.")
                      (current-continuation-marks))])
   (define argv.0 (var "argv" "0"))
   (define automaton.output (var "automaton" "output"))
   (define fallback-port #f)
   (define output-port
     (cond [(non-empty-string? argv.0)
            (info "argv.0 = [~A] will be used as output path."
                  (shorten argv.0))
            (open-output-file argv.0 #:mode 'text #:exists 'replace)]
           [(non-empty-string? automaton.output)
            (warn "argv.0 is not provided.")
            (info "automaton.output = [~A] will be used as output path."
                  (shorten automaton.output))
            (open-output-file automaton.output #:mode 'text #:exists 'replace)]
           [else (warn "Both argv.0 and automaton.output are not provided.")
                 (info "Direct output to current port.")
                 (set! fallback-port (open-output-string))
                 fallback-port]))
   (write-automaton->graphviz automaton output-port var)
   (info "Output completes.")
   (when fallback-port
     (info (get-output-string fallback-port)))))

(provide-handler automaton)
