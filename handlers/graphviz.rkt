#lang racket/base

(require "utils.rkt")
(require "../exn.rkt")
(require "../runtime.rkt")
(require "../lwg-runtime.rkt")
(require racket/string)
(require racket/list)

(define (string->graphviz-id str)
  (define content
    (string-replace str "\"" "\\\""))
  (if (string-suffix? content "\\")
      (raise (exn:fail:lwg-runtime-error
              "Graphviz string cannot end with backslash"
              (current-continuation-marks)))
      (format "\"~A\"" content)))

(struct graphviz-graph
  ([directed? #:mutable]
   [node-list #:mutable]
   [edge-list #:mutable]
   graph-default-attr
   node-default-attr
   edge-default-attr
   attr))

(define (init-graphviz-graph)
  (graphviz-graph #t null null
                  (make-hash) (make-hash)
                  (make-hash) (make-hash)))

(define (add-node! g-graph node)
  (set-graphviz-graph-node-list!
   g-graph
   (pushnew node (graphviz-graph-node-list g-graph))))

(define (add-edge! g-graph node-left node-right)
  (set-graphviz-graph-edge-list!
   g-graph
   (pushnew (list node-left node-right)
            (graphviz-graph-edge-list g-graph))))

(define (add-graph-attr! g-graph attr-name attr-value)
  (hash-set! (graphviz-graph-graph-default-attr g-graph)
             attr-name attr-value))

(define (add-node-attr! g-graph attr-name attr-value)
  (hash-set! (graphviz-graph-node-default-attr g-graph)
             attr-name attr-value))

(define (add-edge-attr! g-graph attr-name attr-value)
  (hash-set! (graphviz-graph-edge-default-attr g-graph)
             attr-name attr-value))

(define (add-attr! g-graph node-or-edge attr-name attr-value)
  (define table (graphviz-graph-attr g-graph))
  (define origin-attr (hash-ref table node-or-edge #hash()))
  (hash-set! table node-or-edge
             (hash-set origin-attr attr-name attr-value)))

(define (write-attr-list g-graph port [key #f] #:use-table [table #f])
  (define attr-table
    (or table (hash-ref (graphviz-graph-attr g-graph) key #f)))
  (when attr-table
    (for ([i (in-naturals)]
          [(attr value) (in-hash attr-table)])
      (if (zero? i)
          (display "\n  [ " port)
          (display "\n  ; " port))
      (fprintf port "~A = ~A"
               (string->graphviz-id attr)
               (string->graphviz-id value)))
    (display " ]" port)))



(define (write-graphviz-graph g-graph port)
  (define edge-op (if (graphviz-graph-directed? g-graph) "->" "--"))
  (if (graphviz-graph-directed? g-graph)
      (displayln "digraph {" port)
      (displayln "graph {" port))
  (unless (hash-empty? (graphviz-graph-graph-default-attr g-graph))
    (display "  graph" port)
    (write-attr-list g-graph port
                     #:use-table (graphviz-graph-graph-default-attr g-graph))
    (displayln ";" port))
  (unless (hash-empty? (graphviz-graph-node-default-attr g-graph))
    (display "  node" port)
    (write-attr-list g-graph port
                     #:use-table (graphviz-graph-node-default-attr g-graph))
    (displayln ";" port))
  (unless (hash-empty? (graphviz-graph-edge-default-attr g-graph))
    (display "  edge" port)
    (write-attr-list g-graph port
                     #:use-table (graphviz-graph-edge-default-attr g-graph))
    (displayln ";" port))
  (for ([node (in-list (graphviz-graph-node-list g-graph))])
    (fprintf port "  ~A" (string->graphviz-id node))
    (write-attr-list g-graph port node)
    (displayln ";" port))
  (for ([edge (in-list (graphviz-graph-edge-list g-graph))])
    (fprintf port "  ~A ~A ~A"
             (string->graphviz-id (first edge)) edge-op
             (string->graphviz-id (second edge)))
    (write-attr-list g-graph port edge)
    (displayln ";" port))
  (displayln "}" port))

(define-handler graphviz
  #:use [#:data graph #:set-data! init-graph!]
  #:use [#:logger/info info #:logger/debug debug #:logger/warning warn]
  #:initially
  (define edge-op
    (and graph (if (graphviz-graph-directed? graph) "->" "--")))
  #:case
  [("file" "start")
   (info "The graph is directed by default.")
   (init-graph! (init-graphviz-graph))]
  #:case
  [("graph" "is" "directed")
   (info "The graph is directed.")
   (set-graphviz-graph-directed?! graph #t)]
  #:case
  [("graph" "is" "undirected")
   (info "The graph is undirected")
   (set-graphviz-graph-directed?! graph #f)]
  #:case
  [("graph" "node" name)
   (debug "* ~A" name)
   (add-node! graph name)]
  #:case
  [("graph" "edge" from-node to-node)
   (debug "- ~A ~A ~A" from-node edge-op to-node)
   (add-edge! graph from-node to-node)]
  #:case
  [("node" name attr = value)
   (debug "@ ~A: [~A = ~A]" name attr (shorten value))
   (add-attr! graph name attr value)]
  #:case
  [("edge" from-node to-node attr = value)
   (debug "@ ~A ~A ~A: [~A = ~A]" from-node edge-op to-node
          attr (shorten value))
   (add-attr! graph (list from-node to-node) attr value)]
  #:case
  [("default" "graph" attr = value)
   (debug "# graph [~A = ~A]" attr (shorten value))
   (add-graph-attr! graph attr value)]
  #:case
  [("default" "node" attr = value)
   (debug "# node [~A = ~A]" attr (shorten value))
   (add-node-attr! graph attr value)]
  #:case
  [("default" "edge" attr = value)
   (debug "# edge [~A = ~A]" attr (shorten value))
   (add-edge-attr! graph attr value)]
  #:use [#:var var]
  #:case
  [("file" "end")
   (define argv.0 (var "argv" "0"))
   (define graphviz.output (var "graphviz" "output"))
   (define fallback-port #f)
   (define output-port
     (cond [(non-empty-string? argv.0)
            (info "argv.0 = [~A] will be used as output path."
                  (shorten argv.0))
            (open-output-file argv.0 #:mode 'text #:exists 'replace)]
           [(non-empty-string? graphviz.output)
            (warn "argv.0 is not provided.")
            (info "graphviz.output = [~A] will be used as output path."
                  (shorten graphviz.output))
            (open-output-file graphviz.output #:mode 'text #:exists 'replace)]
           [else (warn "Both argv.0 and graphviz.output are not provided.")
                 (info "Direct output to current port.")
                 (set! fallback-port (open-output-string))
                 fallback-port]))
   (write-graphviz-graph graph output-port)
   (info "Output completes.")
   (when fallback-port
     (info (get-output-string fallback-port)))])
(provide-handler graphviz)
