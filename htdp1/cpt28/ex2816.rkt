;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2816) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Node is a Symbol
; A Neigbourlist is (cons Node [List-of Node])
; A Graph is [List-of Neighbourlist]]

(define Graph 
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(define Cyclic-graph
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

;; find-route : node node graph  ->  (listof node) or false
;; to create a path from origination to destination in G
;; if there is no path, the function produces false

(check-expect (find-route 'C 'G Graph) false)
(check-expect (find-route 'A 'G Graph) (list 'A 'B 'E 'F 'G))

(define (find-route origination destination G)
  (local (; neighbours: Node -> [Node]
          ; Get a list of the neighbours of node n
          ; ASSUMPTION: Nodes are unique
          (define (neighbours n)
            (local ((define results
                      (filter (lambda (neighbourlist)
                                (symbol=? n (first neighbourlist))) G)))
              (cond
                [(empty? results) (error "Node doesn't exist.")]
                [else (second (first results))])))
          
          ;; find-route/list : (listof node) ->  (listof node) or false
          ;; to create a path from some node on lo-Os to destination
          ;; if there is no path, the function produces false
          (define (find-route/list lo-Os)
            (cond
              [(empty? lo-Os) false]
              [else (local ((define possible-route (find-route (first lo-Os) destination G)))
                      (cond
                        [(boolean? possible-route) (find-route/list (rest lo-Os))]
                        [else possible-route]))]))
          )
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define possible-route 
                      (find-route/list (neighbours origination))))
              (cond
                [(boolean? possible-route) false]
                [else (cons origination possible-route)]))])))

(define gr0 '((A (B))
              (B (A))))

; all-routes: Graph -> [[Node]]
; Test the paths between each node in a graph

(check-expect (all-routes gr0)
              (list (list (find-route 'A 'A gr0)
                          (find-route 'A 'B gr0))
                    (list (find-route 'B 'A gr0)
                          (find-route 'B 'B gr0))))

(define (all-routes g)
  (local ((define nodes (map first g))
          
          ; routes: Node [Nodes] -> [Node]
          ; Test the paths from origin to destinations
          (define (routes origin destinations)
            (map (lambda (destination)
                   (find-route origin destination g)) destinations)))
    (foldr
     (lambda (node paths) (cons (routes node nodes) paths))
     empty
     nodes)))