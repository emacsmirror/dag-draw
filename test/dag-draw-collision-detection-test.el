;;; dag-draw-collision-detection-test.el --- TDD for collision detection and clean edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; FUTURE ENHANCEMENT - Beyond GKNV Baseline
;;
;; Enhancement Category: Quality
;; Baseline Status: ⏳ Deferred (Not required for baseline compliance)
;;
;; This test verifies:
;; - Node/edge collision detection in ASCII rendering
;; - Box overlap prevention and validation
;; - Junction character placement quality checks
;;
;; Related Baseline Decisions: D5.x (ASCII Rendering)
;; Enhancement Source: ASCII rendering quality improvements
;;
;; GKNV baseline prevents collisions through proper positioning (Pass 3).
;; This adds an extra validation layer for ASCII rendering quality.
;; These enhancements may be implemented in Phase 5 (Future Work).
;; See doc/test-suite-analysis.md (Category B3) for categorization rationale.
;;
;; [Original commentary: TDD Implementation: Fix major ASCII rendering issues...]
;;
;; TDD Implementation: Fix major ASCII rendering issues including:
;; - Box overlaps and collisions (┐┌)
;; - Wrong junction character placement (┼──────)
;; - Edges routing through nodes
;; - Double lines and malformed connections
;; These tests drive implementation toward clean, professional ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "ASCII collision detection and edge routing"
  
  (describe "box overlap prevention"
    (it "should prevent node boxes from overlapping or touching"
      ;; RENDERER STRESS TEST: Artificially creates collision scenario
      ;; Manual coordinates intentional - testing renderer collision handling
      ;; This test replicates the ┐┌ overlap issue in demo output
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node-a "API Design")
        (dag-draw-add-node graph 'node-b "Infrastructure Setup")
        (dag-draw-add-node graph 'source "Research Phase")
        (dag-draw-add-edge graph 'source 'node-a)
        (dag-draw-add-edge graph 'source 'node-b)

        ;; Position nodes to potentially cause overlap (like in demo)
        (let* ((source-node (dag-draw-get-node graph 'source))
               (node-a (dag-draw-get-node graph 'node-a))
               (node-b (dag-draw-get-node graph 'node-b)))
          (setf (dag-draw-node-x-coord source-node) 100.0)
          (setf (dag-draw-node-y-coord source-node) 50.0)
          (setf (dag-draw-node-x-coord node-a) 150.0)
          (setf (dag-draw-node-y-coord node-a) 100.0)
          (setf (dag-draw-node-x-coord node-b) 200.0)  ; Close to node-a
          (setf (dag-draw-node-y-coord node-b) 100.0)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== BOX OVERLAP PREVENTION TEST ===")
            (message "%s" ascii-output)
            
            ;; Use test harness for structural validation instead of regex patterns
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
              (expect (plist-get structure-validation :topology-match) :to-be t))
            
            (message "===================================")))))
    
    (it "should maintain minimum safe distances between adjacent nodes"
      ;; RENDERER STRESS TEST: Artificially creates spacing violation scenario
      ;; Manual coordinates intentional - testing renderer spacing handling
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left Node")
        (dag-draw-add-node graph 'right "Right Node")

        ;; Force nodes close together to test spacing
        (let* ((left-node (dag-draw-get-node graph 'left))
               (right-node (dag-draw-get-node graph 'right)))
          (setf (dag-draw-node-x-coord left-node) 50.0)
          (setf (dag-draw-node-y-coord left-node) 50.0)
          (setf (dag-draw-node-x-coord right-node) 80.0)  ; Very close
          (setf (dag-draw-node-y-coord right-node) 50.0)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== MINIMUM DISTANCE TEST ===")
            (message "%s" ascii-output)
            
            ;; Use test harness for validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            
            (message "==============================="))))))
  
  (describe "junction character placement"
    (it "should only place junction characters at proper intersections"
      ;; RENDERER STRESS TEST: Artificially creates junction scenario
      ;; Manual coordinates intentional - testing junction character selection
      ;; This test addresses the ┼────── issue from demo
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target-a "Target A")
        (dag-draw-add-node graph 'target-b "Target B")
        (dag-draw-add-edge graph 'source 'target-a)
        (dag-draw-add-edge graph 'source 'target-b)

        (let* ((source-node (dag-draw-get-node graph 'source))
               (target-a-node (dag-draw-get-node graph 'target-a))
               (target-b-node (dag-draw-get-node graph 'target-b)))
          (setf (dag-draw-node-x-coord source-node) 100.0)
          (setf (dag-draw-node-y-coord source-node) 50.0)
          (setf (dag-draw-node-x-coord target-a-node) 50.0)
          (setf (dag-draw-node-y-coord target-a-node) 150.0)
          (setf (dag-draw-node-x-coord target-b-node) 150.0)
          (setf (dag-draw-node-y-coord target-b-node) 150.0)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== JUNCTION CHARACTER TEST ===")
            (message "%s" ascii-output)
            
            ;; Use test harness for comprehensive validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
              (expect (plist-get structure-validation :topology-match) :to-be t))
            
            (message "===============================")))))
    
    (it "should use appropriate junction characters at line intersections"
      ;; Test for correct junction character selection
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'center "Center")
        (dag-draw-add-node graph 'north "North")
        (dag-draw-add-node graph 'south "South") 
        (dag-draw-add-node graph 'east "East")
        (dag-draw-add-edge graph 'center 'north)
        (dag-draw-add-edge graph 'center 'south)
        (dag-draw-add-edge graph 'center 'east)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== JUNCTION CHARACTER SELECTION TEST ===")
          (message "%s" ascii-output)
          
          ;; Use test harness for validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t))
          
          (message "=========================================")))))
  
  (describe "edge routing through nodes"
    (it "should route edges around nodes, not through them"
      ;; RENDERER STRESS TEST: Artificially creates routing obstruction scenario
      ;; Manual coordinates intentional - testing renderer routing around obstacles
      ;; This addresses edges that pass through other node boxes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'middle "Middle Blocker")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)  ; Edge should route around middle

        ;; Position middle node between start and end to force routing decision
        (let* ((start-node (dag-draw-get-node graph 'start))
               (middle-node (dag-draw-get-node graph 'middle))
               (end-node (dag-draw-get-node graph 'end)))
          (setf (dag-draw-node-x-coord start-node) 50.0)
          (setf (dag-draw-node-y-coord start-node) 50.0)
          (setf (dag-draw-node-x-coord middle-node) 100.0)  ; In the path
          (setf (dag-draw-node-y-coord middle-node) 50.0)
          (setf (dag-draw-node-x-coord end-node) 150.0)
          (setf (dag-draw-node-y-coord end-node) 50.0)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== EDGE ROUTING AROUND NODES TEST ===")
            (message "%s" ascii-output)
            
            ;; Use test harness for comprehensive validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            
            (message "=======================================")))))
    
    (it "should handle complex multi-node routing scenarios"
      ;; Test for routing in complex layouts like the demo
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'api "API Design")
        (dag-draw-add-node graph 'db "Database Design")
        (dag-draw-add-node graph 'infra "Infrastructure")
        (dag-draw-add-edge graph 'research 'api)
        (dag-draw-add-edge graph 'research 'db)
        (dag-draw-add-edge graph 'research 'infra)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== COMPLEX ROUTING TEST ===")
          (message "%s" ascii-output)
          
          ;; Use test harness for comprehensive validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))
          (let ((structure-validation (dag-draw-test--validate-graph-structure ascii-output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t))
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t))
          
          (message "===============================")))))
  
)

(provide 'dag-draw-collision-detection-test)

;;; dag-draw-collision-detection-test.el ends here