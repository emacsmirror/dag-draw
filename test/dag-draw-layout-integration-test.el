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

)

;;; dag-draw-layout-integration-test.el ends here