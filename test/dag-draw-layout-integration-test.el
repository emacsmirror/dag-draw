;;; dag-draw-layout-integration-test.el --- Integration tests for layout->render pipeline -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Integration tests that bridge the gap between layout and rendering.
;; These tests help identify why acceptance tests pass but end-to-end tests fail.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Layout-Render Integration Tests"

  (describe "Layout coordinate output"
    
    (it "should produce usable coordinates from layout pipeline"
        (let ((graph (dag-draw-create-graph)))
          ;; Create the same simple graph as end-to-end test
          (dag-draw-add-node graph 'research "Research")
          (dag-draw-add-node graph 'database "Database")
          (dag-draw-add-edge graph 'research 'database)
          
          ;; Run the layout pipeline (same as end-to-end test)
          (dag-draw-layout-graph graph)
          
          ;; Verify nodes have coordinates after layout
          (let ((research-node (dag-draw-get-node graph 'research))
                (database-node (dag-draw-get-node graph 'database)))
            (expect (dag-draw-node-x-coord research-node) :to-be-truthy)
            (expect (dag-draw-node-y-coord research-node) :to-be-truthy)
            (expect (dag-draw-node-x-coord database-node) :to-be-truthy)
            (expect (dag-draw-node-y-coord database-node) :to-be-truthy)
            
            ;; Debug: Show actual coordinates and node sizes
            (message "Layout coordinates: Research=(%s,%s) size=(%sx%s)"
                     (dag-draw-node-x-coord research-node)
                     (dag-draw-node-y-coord research-node)
                     (dag-draw-node-x-size research-node)
                     (dag-draw-node-y-size research-node))
            (message "                   Database=(%s,%s) size=(%sx%s)"
                     (dag-draw-node-x-coord database-node)
                     (dag-draw-node-y-coord database-node)
                     (dag-draw-node-x-size database-node)
                     (dag-draw-node-y-size database-node))))))

  (describe "Direct rendering test with layout coordinates"
    
    (it "should produce ASCII output with edges using layout coordinates"
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'research "Research")
          (dag-draw-add-node graph 'database "Database") 
          (dag-draw-add-edge graph 'research 'database)
          
          ;; Run layout (same as end-to-end test)
          (dag-draw-layout-graph graph)
          
          ;; Test port calculation with layout coordinates
          (let* ((edge (car (dag-draw-graph-edges graph)))
                 (connection-points (dag-draw--get-edge-connection-points graph edge)))
            (if connection-points
                (let* ((from-port (car connection-points))
                       (to-port (cadr connection-points))
                       (bounds (dag-draw-get-graph-bounds graph))
                       (min-x (nth 0 bounds))
                       (min-y (nth 1 bounds))
                       (scale 2)
                       (from-grid (dag-draw--world-point-to-grid from-port min-x min-y scale))
                       (to-grid (dag-draw--world-point-to-grid to-port min-x min-y scale)))
                  (message "Port calculation SUCCESS: from=(%s,%s) to=(%s,%s)"
                           (dag-draw-point-x from-port) (dag-draw-point-y from-port)
                           (dag-draw-point-x to-port) (dag-draw-point-y to-port))
                  (message "Grid conversion: from=(%s,%s) to=(%s,%s)"
                           (dag-draw-point-x from-grid) (dag-draw-point-y from-grid)
                           (dag-draw-point-x to-grid) (dag-draw-point-y to-grid))
                  (message "Grid bounds: min-x=%s min-y=%s scale=%s" min-x min-y scale))
              (message "Port calculation FAILED: returned nil")))
          
          ;; Render ASCII (same as end-to-end test)
          (let ((ascii-output (dag-draw-render-ascii graph)))
            
            ;; This SHOULD show edges but currently doesn't in end-to-end test
            (message "Integration test ASCII output:")
            (message "%s" ascii-output)
            
            ;; Check for edge characters and debug exactly what's found
            (let ((vertical-match (string-match-p "│" ascii-output))
                  (horizontal-match (string-match-p "─" ascii-output)))
              
              (message "Edge character debug:")
              (message "  Vertical (│) found at position: %s" vertical-match)
              (message "  Horizontal (─) found at position: %s" horizontal-match)
              
              (let ((has-edges (or vertical-match horizontal-match)))
                (if has-edges
                    (message "SUCCESS: Integration test shows edges!")
                  (message "FAILURE: Integration test missing edges - need to debug"))
                
                ;; This expectation will initially fail - that's the integration bug we need to fix
                (expect has-edges :to-be-truthy))))))))

;;; dag-draw-layout-integration-test.el ends here