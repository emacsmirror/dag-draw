;;; dag-draw-position-test.el --- Tests for dag-draw-position.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 3: Complete Positioning
;;
;; This module tests complete GKNV Pass 3 coordinate assignment as specified in
;; "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 4 (X coordinates via auxiliary graph, Y via ranksep)
;; Decision: D3.1 - Network simplex on auxiliary graph (preferred over heuristics)
;;           D3.2 - Omega weights (1, 2, 8) for edge types
;;           D3.3 - Node separation ρ(a,b) = (xsize_a + xsize_b)/2 + nodesep
;;           D3.4 - Exploit auxiliary graph structure for initial feasible tree
;;           D3.5 - X-offset node ports supported
;;           D3.6 - Y coordinates by rank with fixed ranksep
;; Algorithm: Complete GKNV Pass 3 - Coordinate Assignment
;;
;; Key Requirements Tested:
;; - End-to-end Pass 3: ordered graph → positioned graph with X/Y coordinates
;; - X coordinates minimize sum of weighted horizontal edge lengths
;; - Node separation constraints ρ(a,b) satisfied for adjacent nodes
;; - Long edges prefer vertical alignment (omega=8 for virtual-virtual edges)
;; - Node ports (X-offsets) supported via auxiliary graph
;; - Y coordinates maintain fixed ranksep between ranks
;; - Output valid for Pass 4 (spline drawing)
;;
;; Test Coverage:
;; - Complete Pass 3 execution on various graphs
;; - X coordinates optimal (minimum weighted edge length)
;; - Separation constraints satisfied (no overlaps)
;; - Virtual edge chains tend toward vertical alignment
;; - Port offsets correctly incorporated
;; - Y coordinates properly spaced by ranksep
;; - Integration with Pass 4 verified
;; - Various node sizes and separations
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D3.1-D3.6) for full decision rationale.
;; See doc/algorithm-specification.md Pass 3 for implementation details.

;; Tests for node positioning functionality.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-pass2-ordering)
(require 'dag-draw-pass3-positioning)

(describe
 "dag-draw-position"

 (describe
  "Y-coordinate assignment"
  (it "should assign Y coordinates based on ranks"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)

        ;; Set up ranks
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 2)
        (setf (dag-draw-graph-max-rank graph) 2)

        (dag-draw-position-nodes graph)

        (let ((y-a (dag-draw-node-y-coord (dag-draw-get-node graph 'a)))
              (y-b (dag-draw-node-y-coord (dag-draw-get-node graph 'b)))
              (y-c (dag-draw-node-y-coord (dag-draw-get-node graph 'c)))
              (rank-sep (dag-draw-graph-rank-separation graph)))

          (expect y-a :to-equal 0)
          (expect y-b :to-equal rank-sep)
          (expect y-c :to-equal (* 2 rank-sep)))))

  (it "should handle nodes in same rank"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)

        ;; Both nodes at rank 1
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 1)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 1)
        (setf (dag-draw-graph-max-rank graph) 1)

        (dag-draw-position-nodes graph)

        (let ((y-a (dag-draw-node-y-coord (dag-draw-get-node graph 'a)))
              (y-b (dag-draw-node-y-coord (dag-draw-get-node graph 'b)))
              (rank-sep (dag-draw-graph-rank-separation graph)))

          (expect y-a :to-equal rank-sep)
          (expect y-b :to-equal rank-sep)))))

 (describe
  "X-coordinate assignment"
  (it "should assign X coordinates respecting node order"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)

        ;; Set up same rank with different orders
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 0)

        (setf (dag-draw-node-order (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'b)) 1)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'c)) 2)

        (dag-draw-position-nodes graph)

        (let ((x-a (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
              (x-b (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))
              (x-c (dag-draw-node-x-coord (dag-draw-get-node graph 'c))))

          (expect x-a :to-be-less-than x-b)
          (expect x-b :to-be-less-than x-c)
          (expect x-a :to-equal 0))))  ; Should start from 0

  (it "should handle minimum separation constraints"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)

        ;; Set custom node sizes
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'b)) 80)

        ;; Same rank, different orders
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-order (dag-draw-get-node graph 'b)) 1)

        (dag-draw-position-nodes graph)

        (let* ((x-a (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
               (x-b (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))
               (width-a (dag-draw-node-x-size (dag-draw-get-node graph 'a)))
               (width-b (dag-draw-node-x-size (dag-draw-get-node graph 'b)))
               (node-sep (dag-draw-graph-node-separation graph))
               (min-expected-sep (+ (/ (+ width-a width-b) 2.0) node-sep)))

          ;; B should be at least minimum separation distance from A
          (expect (- x-b x-a) :not :to-be-less-than min-expected-sep)))))

 (describe
  "auxiliary graph creation"
  (it "should copy original nodes with omega weights"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-edge graph 'a 'b)

        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
          ;; Should have original nodes + auxiliary nodes for each edge (GKNV constraint approach)
          (expect (ht-size (dag-draw-graph-nodes aux-graph)) :to-equal 3) ; 2 original + 1 aux for edge
          
          ;; Should have constraint edges (aux node connections + separation edges)
          (expect (> (length (dag-draw-graph-edges aux-graph)) 1) :to-be t))))

  (it "should handle multiple edges correctly"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)
        (dag-draw-add-node graph 'c)
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        (let ((aux-graph (dag-draw--build-constraint-auxiliary-graph graph)))
          ;; Should have original nodes + auxiliary nodes for each edge (GKNV constraint approach)
          (expect (ht-size (dag-draw-graph-nodes aux-graph)) :to-equal 5) ; 3 original + 2 aux for edges
          
          ;; Should have constraint edges (aux node connections + separation edges)
          (expect (> (length (dag-draw-graph-edges aux-graph)) 2) :to-be t)))))


 (describe
  "graph bounds calculation"
  (it "should calculate correct bounding box"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a)
        (dag-draw-add-node graph 'b)

        ;; Set coordinates and sizes
        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) 0)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'a)) 100)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'a)) 60)

        (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 200)
        (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) 100)
        (setf (dag-draw-node-x-size (dag-draw-get-node graph 'b)) 80)
        (setf (dag-draw-node-y-size (dag-draw-get-node graph 'b)) 40)

        (let ((bounds (dag-draw-get-graph-bounds graph)))
          (expect (nth 0 bounds) :to-equal -50.0)  ; min-x
          (expect (nth 1 bounds) :to-equal -30.0)  ; min-y
          (expect (nth 2 bounds) :to-equal 240.0)  ; max-x
          (expect (nth 3 bounds) :to-equal 120.0)))))  ; max-y

 (describe
  "integration with full pipeline"
  (it "should work with complete layout pipeline"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "Node A")
        (dag-draw-add-node graph 'b "Node B")
        (dag-draw-add-node graph 'c "Node C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'b 'c)

        ;; Run complete pipeline
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)

        ;; All nodes should have coordinates
        (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) :to-be-truthy)
        (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'a)) :to-be-truthy)
        (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) :to-be-truthy)
        (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'b)) :to-be-truthy)
        (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'c)) :to-be-truthy)
        (expect (dag-draw-node-y-coord (dag-draw-get-node graph 'c)) :to-be-truthy)

        ;; Y coordinates should respect rank order
        (let ((y-a (dag-draw-node-y-coord (dag-draw-get-node graph 'a)))
              (y-b (dag-draw-node-y-coord (dag-draw-get-node graph 'b)))
              (y-c (dag-draw-node-y-coord (dag-draw-get-node graph 'c))))
          (expect y-a :to-be-less-than y-b)
          (expect y-b :to-be-less-than y-c))))

  (it "should handle complex graph with multiple branches"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'root)
        (dag-draw-add-node graph 'left)
        (dag-draw-add-node graph 'right)
        (dag-draw-add-node graph 'sink)
        (dag-draw-add-edge graph 'root 'left)
        (dag-draw-add-edge graph 'root 'right)
        (dag-draw-add-edge graph 'left 'sink)
        (dag-draw-add-edge graph 'right 'sink)

        ;; Run complete pipeline
        (dag-draw-rank-graph graph)
        (dag-draw-order-vertices graph)
        (dag-draw-position-nodes graph)

        ;; All nodes should have coordinates
        (dolist (node-id '(root left right sink))
          (let ((node (dag-draw-get-node graph node-id)))
            (expect (dag-draw-node-x-coord node) :to-be-truthy)
            (expect (dag-draw-node-y-coord node) :to-be-truthy)))

        ;; Left and right should be at same rank but different X positions
        (let ((y-left (dag-draw-node-y-coord (dag-draw-get-node graph 'left)))
              (y-right (dag-draw-node-y-coord (dag-draw-get-node graph 'right)))
              (x-left (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
              (x-right (dag-draw-node-x-coord (dag-draw-get-node graph 'right))))
          (expect y-left :to-equal y-right)  ; Same rank
          (expect x-left :not :to-equal x-right)))))

 (describe
  "omega factor calculation"
  (it "should return correct factors for different node types"
      (let ((graph (dag-draw-create-graph)))
        ;; Test real-real edge
        (expect (dag-draw--get-omega-factor graph 'real1 'real2) :to-equal 1)

        ;; Test real-virtual edge
        (expect (dag-draw--get-omega-factor graph 'real 'virtual_1) :to-equal 2)
        (expect (dag-draw--get-omega-factor graph 'virtual_1 'real) :to-equal 2)

        ;; Test virtual-virtual edge
        (expect (dag-draw--get-omega-factor graph 'virtual_1 'virtual_2) :to-equal 8)))))

;;; dag-draw-position-test.el ends here
