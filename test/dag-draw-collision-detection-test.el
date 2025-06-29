;;; dag-draw-collision-detection-test.el --- TDD for collision detection and clean edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Implementation: Fix major ASCII rendering issues including:
;; - Box overlaps and collisions (┐┌)
;; - Wrong junction character placement (┼──────)
;; - Edges routing through nodes
;; - Double lines and malformed connections
;; These tests drive implementation toward clean, professional ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

(describe "ASCII collision detection and edge routing"
  
  (describe "box overlap prevention"
    (it "should prevent node boxes from overlapping or touching"
      ;; This test replicates the ┐┌ overlap issue in demo output
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node-a "API Design")
        (dag-draw-add-node graph 'node-b "Infrastructure Setup")
        (dag-draw-add-node graph 'source "Research Phase")
        (dag-draw-add-edge graph 'source 'node-a)
        (dag-draw-add-edge graph 'source 'node-b)
        (dag-draw-layout-graph graph)
        
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
            
            ;; EXPECTATION: Should NOT have box overlap patterns
            (expect ascii-output :not :to-match "┐┌")     ; No touching right-left corners
            (expect ascii-output :not :to-match "┘└")     ; No touching bottom corners
            (expect ascii-output :not :to-match "┤├")     ; No side-by-side box edges
            
            ;; Should have clean separation with proper spacing
            (expect ascii-output :to-match "API Design")
            (expect ascii-output :to-match "Infrastructure Setup")
            
            (message "===================================")))))
    
    (it "should maintain minimum safe distances between adjacent nodes"
      ;; Test for proper spacing algorithm
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left Node")
        (dag-draw-add-node graph 'right "Right Node")
        (dag-draw-layout-graph graph)
        
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
            
            ;; Should have at least 2-3 character spacing between boxes
            (expect ascii-output :to-match "Left Node")
            (expect ascii-output :to-match "Right Node")
            
            ;; Should NOT have collision patterns
            (expect ascii-output :not :to-match "┐┌")
            (expect ascii-output :not :to-match "e  ├")  ; Text running into box
            
            (message "==============================="))))))
  
  (describe "junction character placement"
    (it "should only place junction characters at proper intersections"
      ;; This test addresses the ┼────── issue from demo
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target-a "Target A")
        (dag-draw-add-node graph 'target-b "Target B")
        (dag-draw-add-edge graph 'source 'target-a)
        (dag-draw-add-edge graph 'source 'target-b)
        (dag-draw-layout-graph graph)
        
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
            
            ;; EXPECTATION: Junction characters should only appear at line intersections
            ;; NOT attached to box edges like ┼──────└─────────────────┘
            
            ;; Should NOT have junction characters directly attached to boxes
            (expect ascii-output :not :to-match "┼──────└")   ; Junction attached to corner
            (expect ascii-output :not :to-match "┼│.*│")      ; Junction attached to side
            (expect ascii-output :not :to-match "┘┼")         ; Junction right after corner
            
            ;; Should have clean connections
            (expect ascii-output :to-match "Source")
            (expect ascii-output :to-match "Target A")
            (expect ascii-output :to-match "Target B")
            
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
        (dag-draw-layout-graph graph)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== JUNCTION CHARACTER SELECTION TEST ===")
          (message "%s" ascii-output)
          
          ;; Should have nodes present
          (expect ascii-output :to-match "Center")
          (expect ascii-output :to-match "North")
          (expect ascii-output :to-match "South")
          (expect ascii-output :to-match "East")
          
          ;; Should NOT have malformed junction patterns
          (expect ascii-output :not :to-match "┼┼")      ; Double junctions
          (expect ascii-output :not :to-match "├┤")      ; Side-by-side T-junctions
          
          (message "=========================================")))))
  
  (describe "edge routing through nodes"
    (it "should route edges around nodes, not through them"
      ;; This addresses edges that pass through other node boxes
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'middle "Middle Blocker")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)  ; Edge should route around middle
        (dag-draw-layout-graph graph)
        
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
            
            ;; All nodes should be visible (not overwritten by edges)
            (expect ascii-output :to-match "Start")
            (expect ascii-output :to-match "Middle Blocker")
            (expect ascii-output :to-match "End")
            
            ;; Should NOT have edges going through node text
            (expect ascii-output :not :to-match "M─iddle")     ; Horizontal line through text
            (expect ascii-output :not :to-match "M│iddle")     ; Vertical line through text
            (expect ascii-output :not :to-match "Bl▼cker")     ; Arrow through text
            
            ;; Should have some routing characters (edge goes around, not through)
            (expect ascii-output :to-match "[│─┌┐└┘├┤┬┴┼]")
            
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
        (dag-draw-layout-graph graph)
        
        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== COMPLEX ROUTING TEST ===")
          (message "%s" ascii-output)
          
          ;; All nodes should be visible and intact
          (expect ascii-output :to-match "Research")
          (expect ascii-output :to-match "API Design")
          (expect ascii-output :to-match "Database Design")
          (expect ascii-output :to-match "Infrastructure")
          
          ;; Should NOT have malformed double-line artifacts (overlapping lines)
          (expect ascii-output :not :to-match "││")        ; Double vertical lines side-by-side
          (expect ascii-output :not :to-match "─\n─")      ; Double horizontal lines stacked
          
          ;; Should have clean connecting lines
          (expect ascii-output :to-match "[│─▼▲▶◀]")
          
          (message "===============================")))))
  
  (describe "occupancy map debugging"
    (it "should debug occupancy map creation and collision detection"
      ;; Debug test to understand why collision detection isn't working
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'start "Start")
        (dag-draw-add-node graph 'middle "Middle")
        (dag-draw-add-node graph 'end "End")
        (dag-draw-add-edge graph 'start 'end)
        (dag-draw-layout-graph graph)
        
        ;; Position nodes to force collision (like failing test)
        (let* ((start-node (dag-draw-get-node graph 'start))
               (middle-node (dag-draw-get-node graph 'middle))
               (end-node (dag-draw-get-node graph 'end)))
          (setf (dag-draw-node-x-coord start-node) 50.0)
          (setf (dag-draw-node-y-coord start-node) 50.0)
          (setf (dag-draw-node-x-coord middle-node) 100.0)
          (setf (dag-draw-node-y-coord middle-node) 50.0)
          (setf (dag-draw-node-x-coord end-node) 150.0)
          (setf (dag-draw-node-y-coord end-node) 50.0)
          
          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== OCCUPANCY MAP DEBUG TEST ===")
            (message "%s" ascii-output)
            
            ;; Debug: Show node coordinates
            (message "Start node: world=(%.1f,%.1f)" 
                     (dag-draw-node-x-coord start-node) (dag-draw-node-y-coord start-node))
            (message "Middle node: world=(%.1f,%.1f)" 
                     (dag-draw-node-x-coord middle-node) (dag-draw-node-y-coord middle-node))
            (message "End node: world=(%.1f,%.1f)" 
                     (dag-draw-node-x-coord end-node) (dag-draw-node-y-coord end-node))
            
            ;; The key insight: if occupancy map worked, we should see all node text intact
            (expect ascii-output :to-match "Start")
            (expect ascii-output :to-match "Middle")
            (expect ascii-output :to-match "End")
            
            (message "==================================")))))))

(provide 'dag-draw-collision-detection-test)

;;; dag-draw-collision-detection-test.el ends here