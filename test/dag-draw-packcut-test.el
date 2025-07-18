;;; dag-draw-packcut-test.el --- TDD tests for GKNV packcut() compaction algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; TDD tests for implementing GKNV packcut() function for layout compaction.
;; Based on GKNV paper specification: "packcut sweeps the layout from left to right, 
;; searching for blocks that can be compacted. For each node, if all the nodes to the 
;; right of it can be shifted to the left by some increment without violating any 
;; positioning constraints, the shift is performed."
;;
;; Reference: "A Technique for Drawing Directed Graphs" by Gansner, Koutsofios, North, Vo
;; Section 4.1: "9: packcut sweeps the layout from left to right, searching for blocks
;; that can be compacted..."

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-pass3-positioning)
(require 'dag-draw-test-helpers)

;;; Layout Compaction Detection Tests

(describe "GKNV packcut() layout compaction detection"
  (it "should identify compactable space between nodes"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph with excessive spacing that can be compacted
      (dag-draw-add-node graph 'left "Left Node")
      (dag-draw-add-node graph 'right "Right Node")
      (dag-draw-add-edge graph 'left 'right)
      
      ;; Set up same rank with excessive spacing
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 500)  ; Excessive gap
      
      ;; Test compaction opportunity detection
      (let ((compaction-ops (dag-draw--find-compaction-opportunities graph)))
        (expect (length compaction-ops) :to-be-greater-than 0)
        (let ((first-op (car compaction-ops)))
          (expect (plist-get first-op :can-compact) :to-be-truthy)
          (expect (plist-get first-op :savings) :to-be-greater-than 100)))))

  (it "should detect no compaction opportunity when nodes properly spaced"
    (let ((graph (dag-draw-create-graph)))
      ;; Create properly spaced nodes
      (dag-draw-add-node graph 'left "Left Node")
      (dag-draw-add-node graph 'right "Right Node")
      (dag-draw-add-edge graph 'left 'right)
      
      ;; Set up minimal valid spacing (using GKNV separation formula)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 200)  ; Minimal spacing
      
      ;; Should find minimal or no compaction opportunities
      (let ((compaction-ops (dag-draw--find-compaction-opportunities graph)))
        (expect (cl-every (lambda (op) 
                           (< (plist-get op :savings) 50))
                         compaction-ops) :to-be-truthy))))

  (it "should handle multiple ranks with different compaction opportunities"
    (let ((graph (dag-draw-create-graph)))
      ;; Create multi-rank graph
      (dag-draw-add-node graph 'top1 "Top 1")
      (dag-draw-add-node graph 'top2 "Top 2") 
      (dag-draw-add-node graph 'bottom1 "Bottom 1")
      (dag-draw-add-node graph 'bottom2 "Bottom 2")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'top1)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'top2)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'bottom1)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'bottom2)) 1)
      
      ;; Set coordinates with different spacing patterns
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top1)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'top2)) 600)    ; Large gap
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom1)) 150)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'bottom2)) 250) ; Small gap
      
      ;; Should detect different compaction potentials per rank
      (let ((compaction-ops (dag-draw--find-compaction-opportunities graph)))
        (expect (length compaction-ops) :to-be-greater-than 0)))))

;;; Layout Compaction Algorithm Tests

(describe "GKNV packcut() layout compaction algorithm"
  (it "should compact simple layout by removing excessive spacing"
    (let ((graph (dag-draw-create-graph)))
      ;; Create layout with compactable spacing
      (dag-draw-add-node graph 'a "Node A")
      (dag-draw-add-node graph 'b "Node B")
      (dag-draw-add-node graph 'c "Node C")
      
      ;; Set up linear layout with excessive spacing
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 400)  ; Big gap
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'c)) 700)  ; Big gap
      
      ;; Apply packcut compaction
      (let ((original-width (dag-draw--calculate-layout-width graph)))
        (dag-draw--packcut-compact-layout graph)
        (let ((compacted-width (dag-draw--calculate-layout-width graph)))
          ;; Layout should be narrower after compaction
          (expect compacted-width :to-be-less-than original-width)
          ;; Nodes should maintain left-to-right order
          (let ((a-x (dag-draw-node-x-coord (dag-draw-get-node graph 'a)))
                (b-x (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))
                (c-x (dag-draw-node-x-coord (dag-draw-get-node graph 'c))))
            (expect a-x :to-be-less-than b-x)
            (expect b-x :to-be-less-than c-x))))))

  (it "should respect GKNV separation constraints during compaction"
    (let ((graph (dag-draw-create-graph)))
      ;; Create nodes that require minimum separation
      (dag-draw-add-node graph 'left "Left Node")
      (dag-draw-add-node graph 'right "Right Node")
      (dag-draw-add-edge graph 'left 'right)  ; Connection requires separation
      
      ;; Set up with excessive spacing
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'left)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'right)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'left)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'right)) 500)
      
      ;; Apply compaction
      (dag-draw--packcut-compact-layout graph)
      
      ;; Verify separation constraint is maintained
      (let* ((left-x (dag-draw-node-x-coord (dag-draw-get-node graph 'left)))
             (right-x (dag-draw-node-x-coord (dag-draw-get-node graph 'right)))
             (actual-separation (- right-x left-x))
             (min-separation (dag-draw--calculate-separation graph 'left 'right)))
        (expect actual-separation :to-be-greater-than (- min-separation 5))))) ; Small tolerance

  (it "should perform left-to-right sweep compaction as specified by GKNV"
    (let ((graph (dag-draw-create-graph)))
      ;; Create layout that benefits from left-to-right sweep
      (dag-draw-add-node graph 'a "A")
      (dag-draw-add-node graph 'b "B") 
      (dag-draw-add-node graph 'c "C")
      (dag-draw-add-node graph 'd "D")
      
      ;; Set up with progressively larger gaps moving right
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'b)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'c)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'd)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'b)) 250)  ; 150 gap
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'c)) 500)  ; 250 gap
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'd)) 900)  ; 400 gap
      
      ;; Track coordinates before compaction
      (let ((positions-before (mapcar (lambda (id) 
                                       (cons id (dag-draw-node-x-coord 
                                               (dag-draw-get-node graph id))))
                                     '(a b c d))))
        
        ;; Apply GKNV left-to-right compaction sweep
        (dag-draw--packcut-compact-layout graph)
        
        ;; Verify compaction happened (rightmost nodes moved left more)
        (let ((d-moved (- (cdr (assoc 'd positions-before))
                         (dag-draw-node-x-coord (dag-draw-get-node graph 'd))))
              (b-moved (- (cdr (assoc 'b positions-before))
                         (dag-draw-node-x-coord (dag-draw-get-node graph 'b)))))
          ;; Rightmost node should move left more than leftmost node
          (expect d-moved :to-be-greater-than b-moved)))))

  (it "should compact multiple ranks independently"
    (let ((graph (dag-draw-create-graph)))
      ;; Create multi-rank layout with different compaction needs
      (dag-draw-add-node graph 'rank0_a "R0 A")
      (dag-draw-add-node graph 'rank0_b "R0 B")
      (dag-draw-add-node graph 'rank1_a "R1 A")  
      (dag-draw-add-node graph 'rank1_b "R1 B")
      
      ;; Set up ranks
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank0_a)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank0_b)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank1_a)) 1)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'rank1_b)) 1)
      
      ;; Set coordinates with different spacing per rank
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'rank0_a)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'rank0_b)) 600) ; Large gap rank 0
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'rank1_a)) 150)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'rank1_b)) 250) ; Small gap rank 1
      
      ;; Apply compaction
      (dag-draw--packcut-compact-layout graph)
      
      ;; Verify rank 0 compacted more than rank 1
      (let ((rank0-width (- (dag-draw-node-x-coord (dag-draw-get-node graph 'rank0_b))
                           (dag-draw-node-x-coord (dag-draw-get-node graph 'rank0_a))))
            (rank1-width (- (dag-draw-node-x-coord (dag-draw-get-node graph 'rank1_b))
                           (dag-draw-node-x-coord (dag-draw-get-node graph 'rank1_a)))))
        ;; Rank 0 had more compaction opportunity, should be more compact now
        (expect rank0-width :to-be-less-than 400)  ; Down from 500
        (expect rank1-width :to-be-less-than 150)))))  ; Should stay similar

;;; Constraint Handling and Edge Cases

(describe "GKNV packcut() constraint handling and edge cases"
  (it "should handle empty graph gracefully"
    (let ((graph (dag-draw-create-graph)))
      ;; Should not error on empty graph
      (expect (dag-draw--packcut-compact-layout graph) :not :to-throw)))

  (it "should handle single node graph"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'single "Single Node")
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'single)) 100)
      
      ;; Should not change single node position
      (dag-draw--packcut-compact-layout graph)
      (expect (dag-draw-node-x-coord (dag-draw-get-node graph 'single)) :to-equal 100)))

  (it "should maintain node ordering during compaction"
    (let ((graph (dag-draw-create-graph)))
      ;; Create nodes in specific order
      (dag-draw-add-node graph 'first "First")
      (dag-draw-add-node graph 'second "Second")
      (dag-draw-add-node graph 'third "Third")
      
      ;; Set up with proper left-to-right ordering but excessive spacing
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'first)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'second)) 0)
      (setf (dag-draw-node-rank (dag-draw-get-node graph 'third)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'first)) 100)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'second)) 400)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'third)) 700)
      
      ;; Apply compaction
      (dag-draw--packcut-compact-layout graph)
      
      ;; Verify ordering preserved
      (let ((first-x (dag-draw-node-x-coord (dag-draw-get-node graph 'first)))
            (second-x (dag-draw-node-x-coord (dag-draw-get-node graph 'second)))
            (third-x (dag-draw-node-x-coord (dag-draw-get-node graph 'third))))
        (expect first-x :to-be-less-than second-x)
        (expect second-x :to-be-less-than third-x))))

  (it "should integrate with GKNV Pass 3 positioning workflow"
    (let ((graph (dag-draw-create-graph)))
      ;; Create graph that goes through full positioning workflow with horizontal spread
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'middle1 "Middle1")
      (dag-draw-add-node graph 'middle2 "Middle2")
      (dag-draw-add-node graph 'target "Target")
      (dag-draw-add-edge graph 'source 'middle1)
      (dag-draw-add-edge graph 'source 'middle2)
      (dag-draw-add-edge graph 'middle1 'target)
      (dag-draw-add-edge graph 'middle2 'target)
      
      ;; Run full GKNV positioning workflow
      (dag-draw-rank-graph graph)
      (dag-draw-order-vertices graph)
      (dag-draw-position-nodes graph)
      
      ;; Apply packcut as final positioning enhancement
      (let ((original-width (dag-draw--calculate-layout-width graph)))
        (dag-draw--packcut-compact-layout graph)
        (let ((compacted-width (dag-draw--calculate-layout-width graph)))
          ;; Should have valid layout after full workflow + compaction
          (expect compacted-width :to-be-greater-than 0)
          ;; All nodes should have valid coordinates
          (ht-each (lambda (node-id node)
                     (expect (dag-draw-node-x-coord node) :not :to-be nil)
                     (expect (dag-draw-node-y-coord node) :not :to-be nil))
                   (dag-draw-graph-nodes graph)))))))

(provide 'dag-draw-packcut-test)

;;; dag-draw-packcut-test.el ends here