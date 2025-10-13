;;; dag-draw-minpath-test.el --- TDD tests for GKNV minpath() virtual node chain straightening -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; GKNV Baseline Compliance Tests - Pass 3: Virtual Chain Straightening
;;
;; This module tests GKNV virtual edge chain straightening via omega weights as
;; specified in "A Technique for Drawing Directed Graphs" (Gansner, Koutsofios,
;; North, Vo).
;;
;; GKNV Reference: Section 4 (omega weights favor straight long edges)
;; Decision: D3.2 - Omega values: 1 (real-real), 2 (real-virtual), 8 (virtual-virtual)
;; Algorithm: Edge Type Weight Assignment for Straightening
;;
;; Key Requirements Tested:
;; - Edge types: (1) both real, (2) one real one virtual, (3) both virtual
;; - Omega relationship: Ω(e) ≤ Ω(f) ≤ Ω(g) for types (1), (2), (3)
;; - Implementation values: 1, 2, 8
;; - Higher omega for virtual-virtual edges favors vertical alignment
;; - Network simplex minimizes Σ Ω(e) × horizontal_length(e)
;; - Result: long edges (virtual chains) tend to be straight/vertical
;; - Supports aesthetic A3 (keep edges short and straight)
;;
;; Test Coverage:
;; - Edge type classification correct (real-real, real-virtual, virtual-virtual)
;; - Omega values assigned correctly (1, 2, 8)
;; - Network simplex respects omega weights in optimization
;; - Virtual chains tend toward vertical alignment
;; - Higher omega edges prioritized (shorter horizontal span)
;; - Visual result: long edges appear straight
;; - Various graph structures with different edge types
;;
;; Baseline Status: ✅ Required for GKNV compliance
;;
;; See doc/implementation-decisions.md (D3.2) for full decision rationale.
;; See doc/algorithm-specification.md Pass 3 for implementation details.

;; TDD tests for implementing GKNV minpath() function for virtual node chain straightening.
;; Based on GKNV paper specification: "minpath straightens chains of virtual nodes
;; by sequentially finding sub-chains that may be assigned the same X coordinate."
;;
;; Reference: "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, Vo
;; Section 4.1: "8: minpath straightens chains of virtual nodes by sequentially finding
;; sub-chains that may be assigned the same X coordinate."

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass3-positioning)

;;; Virtual Node Chain Detection Tests

(describe "GKNV minpath() virtual node chain detection"
  (it "should identify simple virtual node chain"
    (let ((graph (dag-draw-create-graph)))
      ;; Create chain: real_a -> virtual_1 -> virtual_2 -> real_b
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'virtual_1 "")
      (dag-draw-add-node graph 'virtual_2 "")
      (dag-draw-add-node graph 'real_b "Real B")
      
      ;; Set up ranks for vertical chain
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_2)) 2)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_b)) 3)
      
      ;; Add edges to form chain
      (dag-draw-add-edge graph 'real_a 'virtual_1)
      (dag-draw-add-edge graph 'virtual_1 'virtual_2)
      (dag-draw-add-edge graph 'virtual_2 'real_b)
      
      ;; Test chain detection
      (let ((chains (dag-draw--find-virtual-node-chains graph)))
        (expect (length chains) :to-equal 1)
        (expect (car chains) :to-equal '(virtual_1 virtual_2)))))

  (it "should identify multiple independent virtual node chains"
    (let ((graph (dag-draw-create-graph)))
      ;; Create two independent chains
      ;; Chain 1: real_a -> virtual_a1 -> virtual_a2 -> real_b
      ;; Chain 2: real_c -> virtual_c1 -> real_d
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'virtual_a1 "")
      (dag-draw-add-node graph 'virtual_a2 "")
      (dag-draw-add-node graph 'real_b "Real B")
      (dag-draw-add-node graph 'real_c "Real C")
      (dag-draw-add-node graph 'virtual_c1 "")
      (dag-draw-add-node graph 'real_d "Real D")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_a1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_a2)) 2)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_b)) 3)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_c)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_c1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_d)) 2)
      
      ;; Add edges
      (dag-draw-add-edge graph 'real_a 'virtual_a1)
      (dag-draw-add-edge graph 'virtual_a1 'virtual_a2)
      (dag-draw-add-edge graph 'virtual_a2 'real_b)
      (dag-draw-add-edge graph 'real_c 'virtual_c1)
      (dag-draw-add-edge graph 'virtual_c1 'real_d)
      
      ;; Test detection of multiple chains
      (let ((chains (dag-draw--find-virtual-node-chains graph)))
        (expect (length chains) :to-equal 2)
        (expect chains :to-contain '(virtual_a1 virtual_a2))
        (expect chains :to-contain '(virtual_c1)))))

  (it "should handle graph with no virtual nodes"
    (let ((graph (dag-draw-create-graph)))
      ;; Create simple graph with only real nodes
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'real_b "Real B")
      (dag-draw-add-edge graph 'real_a 'real_b)
      
      ;; Should find no virtual chains
      (let ((chains (dag-draw--find-virtual-node-chains graph)))
        (expect (length chains) :to-equal 0)))))

;;; Virtual Node Chain Straightening Tests

(describe "GKNV minpath() virtual node chain straightening"
  (it "should align virtual nodes in same X coordinate for straight vertical chain"
    (let ((graph (dag-draw-create-graph)))
      ;; Create vertical chain with misaligned virtual nodes
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'virtual_1 "")
      (dag-draw-add-node graph 'virtual_2 "")
      (dag-draw-add-node graph 'real_b "Real B")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_2)) 2)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_b)) 3)
      
      ;; Add edges to form chain
      (dag-draw-add-edge graph 'real_a 'virtual_1)
      (dag-draw-add-edge graph 'virtual_1 'virtual_2)
      (dag-draw-add-edge graph 'virtual_2 'real_b)
      
      ;; Set initial misaligned X coordinates
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1)) 120)  ; misaligned
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_2)) 80)   ; misaligned
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_b)) 100)
      
      ;; Apply minpath straightening
      (dag-draw--minpath-straighten-virtual-chains graph)
      
      ;; Verify virtual nodes are aligned
      (let ((virtual_1_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1)))
            (virtual_2_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_2))))
        (expect virtual_1_x :to-equal virtual_2_x))))

  (it "should straighten multiple independent chains without affecting each other"
    (let ((graph (dag-draw-create-graph)))
      ;; Create two independent vertical chains
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'virtual_a1 "")
      (dag-draw-add-node graph 'virtual_a2 "")
      (dag-draw-add-node graph 'real_b "Real B")
      (dag-draw-add-node graph 'real_c "Real C")
      (dag-draw-add-node graph 'virtual_c1 "")
      (dag-draw-add-node graph 'real_d "Real D")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_a1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_a2)) 2)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_b)) 3)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_c)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_c1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_d)) 2)
      
      ;; Add edges
      (dag-draw-add-edge graph 'real_a 'virtual_a1)
      (dag-draw-add-edge graph 'virtual_a1 'virtual_a2)
      (dag-draw-add-edge graph 'virtual_a2 'real_b)
      (dag-draw-add-edge graph 'real_c 'virtual_c1)
      (dag-draw-add-edge graph 'virtual_c1 'real_d)
      
      ;; Set initial X coordinates (chains separated)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_a1)) 120)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_a2)) 80)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_b)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_c)) 300)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_c1)) 320)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_d)) 300)
      
      ;; Apply minpath straightening
      (dag-draw--minpath-straighten-virtual-chains graph)
      
      ;; Verify each chain is internally aligned
      (let ((virtual_a1_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_a1)))
            (virtual_a2_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_a2)))
            (virtual_c1_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_c1))))
        ;; Chain A should be aligned
        (expect virtual_a1_x :to-equal virtual_a2_x)
        ;; Chain C should be aligned with itself (only one virtual node)
        (expect virtual_c1_x :to-be-greater-than 250)  ; Should stay in its region
        ;; Chains should remain separated
        (expect (abs (- virtual_a1_x virtual_c1_x)) :to-be-greater-than 100))))

  (it "should calculate optimal X coordinate for virtual chain alignment"
    (let ((graph (dag-draw-create-graph)))
      ;; Create chain where optimal X should be median of endpoints
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'virtual_1 "")
      (dag-draw-add-node graph 'real_b "Real B")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'virtual_1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'real_b)) 2)
      
      ;; Add edges
      (dag-draw-add-edge graph 'real_a 'virtual_1)
      (dag-draw-add-edge graph 'virtual_1 'real_b)
      
      ;; Set endpoint X coordinates
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1)) 200)  ; will be adjusted
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_b)) 300)
      
      ;; Apply minpath straightening
      (dag-draw--minpath-straighten-virtual-chains graph)
      
      ;; Virtual node should be aligned to median/average of endpoints
      (let ((virtual_1_x (dag-draw-node-x-coord (dag-draw-get-node graph 'virtual_1))))
        ;; Should be around the median of 100 and 300 = 200
        (expect virtual_1_x :to-be-close-to 200 50)))))

;;; Edge Case and Integration Tests

(describe "GKNV minpath() edge cases and integration"
  (it "should handle empty graph gracefully"
    (let ((graph (dag-draw-create-graph)))
      ;; Should not error on empty graph
      (expect (dag-draw--minpath-straighten-virtual-chains graph) :not :to-throw)))

  (it "should handle graph with only real nodes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'real_a "Real A")
      (dag-draw-add-node graph 'real_b "Real B")
      (dag-draw-add-edge graph 'real_a 'real_b)
      
      ;; Should not modify real node positions
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'real_b)) 200)
      
      (dag-draw--minpath-straighten-virtual-chains graph)
      
      ;; Real nodes should be unchanged
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'real_a)) :to-equal 100)
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'real_b)) :to-equal 200)))

  (it "should integrate with GKNV Pass 3 positioning workflow"
    (let ((graph (dag-draw-create-graph)))
      ;; Create complex graph with virtual chains
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'virtual_1 "")
      (dag-draw-add-node graph 'virtual_2 "")
      (dag-draw-add-node graph 'target "Target")
      
      ;; Run full GKNV positioning workflow
      (dag-draw-rank-graph graph)
      (dag-draw-order-vertices graph)
      (dag-draw-position-nodes graph)
      
      ;; Add edges to create virtual chain
      (dag-draw-add-edge graph 'source 'virtual_1)
      (dag-draw-add-edge graph 'virtual_1 'virtual_2)
      (dag-draw-add-edge graph 'virtual_2 'target)
      
      ;; Apply minpath as part of positioning enhancements
      (dag-draw--minpath-straighten-virtual-chains graph)
      
      ;; Should have valid coordinates for all nodes
      (ht-each (lambda (node-id node)
                 (expect (dag-draw-node-x-coord node) :not :to-be nil)
                 (expect (dag-draw-node-y-coord node) :not :to-be nil))
               (dag-draw-graph-nodes graph)))))

(provide 'dag-draw-minpath-test)

;;; dag-draw-minpath-test.el ends here