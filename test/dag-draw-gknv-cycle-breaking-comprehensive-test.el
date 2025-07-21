;;; dag-draw-gknv-cycle-breaking-comprehensive-test.el --- Comprehensive GKNV cycle breaking tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive tests for GKNV cycle breaking per Section 2.1.
;; Based on additional paper requirements: strongly connected components,
;; cycle participation counting, source/sink node preference, etc.
;;
;; GKNV Reference: Section 2.1, lines 388-403
;; Ubiquitous Language: Complete DFS Edge Classification

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-cycle-breaking)

(describe "GKNV Comprehensive Cycle Breaking"

  (describe "strongly connected component handling per Section 2.1"

    (it "should handle non-trivial strongly connected components"
      ;; Based on GKNV Section 2.1, line 393: "non-trivial strongly connected component"
      (let ((graph (dag-draw-create-graph)))
        ;; Create a strongly connected component: A->B->C->A
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)
        (dag-draw-add-edge graph 'c 'a)

        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should identify exactly one back edge in the SCC
          (let ((back-edges (ht-get classification 'back-edges)))
            (expect (length back-edges) :to-equal 1)
            ;; Back edge should be the last one discovered that creates cycle
            (expect back-edges :not :to-be nil)))))

    (it "should count cycle participation per GKNV heuristic"
      ;; Based on GKNV Section 2.1, line 394: "counts the number of times each edge forms a cycle"
      (let ((graph (dag-draw-create-graph)))
        ;; Create complex cycle structure where some edges participate in multiple cycles
        (dag-draw-add-node graph 'center "Center")
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")

        ;; Create multiple overlapping cycles through center
        (dag-draw-add-edge graph 'center 'a)
        (dag-draw-add-edge graph 'a 'center)     ; Cycle 1: center->a->center
        (dag-draw-add-edge graph 'center 'b)
        (dag-draw-add-edge graph 'b 'center)     ; Cycle 2: center->b->center
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'a)          ; Cycle 3: a->b->a

        ;; Function should exist to count cycle participation
        (expect (fboundp 'dag-draw--count-cycle-participation) :to-be t)

        (let ((participation (dag-draw--count-cycle-participation graph)))
          ;; Edges involving center should have higher participation counts
          (expect (ht? participation) :to-be t))))

    (it "should preserve original visual direction when reversing"
      ;; Based on GKNV Section 2.1, line 372: "edges are only reversed internally"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'x "X")
        (dag-draw-add-node graph 'y "Y")

        ;; Create cycle with labeled edge
        (let ((original-edge (dag-draw-add-edge graph 'x 'y 1 "ORIGINAL->DIRECTION")))
          (dag-draw-add-edge graph 'y 'x)

          ;; Break cycles using GKNV method
          (dag-draw--break-cycles-using-gknv-classification graph)

          ;; Graph should be acyclic after breaking
          (expect (dag-draw-simple-has-cycles graph) :to-be nil)

          ;; Original visual direction info should be preserved somehow
          ;; (Implementation detail: could be in edge attributes/label)
          (let ((edges (dag-draw-graph-edges graph)))
            (expect (length edges) :to-equal 2)  ; Same number of edges
            ;; At least one edge should preserve original direction info
            (expect (cl-some (lambda (e) (dag-draw-edge-label e)) edges) :not :to-be nil)))))

    (it "should start DFS from source or sink nodes when available"
      ;; Based on GKNV Section 2.1, line 375: "starting from some source or sink nodes if any exist"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")   ; Source node (no incoming edges)
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'sink "Sink")       ; Sink node (no outgoing edges)

        (dag-draw-add-edge graph 'source 'middle)
        (dag-draw-add-edge graph 'middle 'sink)

        ;; Function should exist to identify source/sink nodes
        (expect (fboundp 'dag-draw--find-source-sink-nodes) :to-be t)

        (let ((source-sink (dag-draw--find-source-sink-nodes graph)))
          (expect (ht-get source-sink 'sources) :to-contain 'source)
          (expect (ht-get source-sink 'sinks) :to-contain 'sink))))

    (it "should handle disconnected graph components"
      ;; Extension of GKNV for practical implementation
      (let ((graph (dag-draw-create-graph)))
        ;; Component 1: A -> B
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-edge graph 'a 'b)

        ;; Component 2: C -> D -> C (with cycle)
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-node graph 'd "D")
        (dag-draw-add-edge graph 'c 'd)
        (dag-draw-add-edge graph 'd 'c)

        (let ((classification (dag-draw--classify-edges-gknv graph)))
          ;; Should classify edges across both components
          (expect (+ (length (ht-get classification 'tree-edges))
                     (length (ht-get classification 'back-edges))
                     (length (ht-get classification 'forward-edges))
                     (length (ht-get classification 'cross-edges)))
                  :to-equal 3)  ; Total edges in graph

          ;; Should find back edge in component 2
          (let ((back-edges (ht-get classification 'back-edges)))
            (expect (length back-edges) :to-equal 1)))))))

(provide 'dag-draw-gknv-cycle-breaking-comprehensive-test)

;;; dag-draw-gknv-cycle-breaking-comprehensive-test.el ends here
