;;; dag-draw-grid-sizing-test.el --- Test grid sizing issues -*- lexical-binding: t -*-

;; Test to reproduce the issue where nodes are positioned outside grid boundaries

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "Grid Sizing Issues"
  (it "should include all nodes within grid boundaries after spline regeneration"
    ;; Test case that reproduces the missing "End" node issue
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle Blocker") 
      (dag-draw-add-node graph 'end "End")
      
      ;; Position nodes horizontally to trigger the coordinate issue
      (dag-draw-add-edge graph 'start 'end)
      (dag-draw-layout-graph graph)
      
      ;; Manually set coordinates to reproduce the test case
      (let ((start-node (dag-draw-get-node graph 'start))
            (middle-node (dag-draw-get-node graph 'middle))
            (end-node (dag-draw-get-node graph 'end)))
        (setf (dag-draw-node-x-coord start-node) 50.0)
        (setf (dag-draw-node-y-coord start-node) 50.0)
        (setf (dag-draw-node-x-coord middle-node) 100.0)
        (setf (dag-draw-node-y-coord middle-node) 50.0)
        (setf (dag-draw-node-x-coord end-node) 150.0)
        (setf (dag-draw-node-y-coord end-node) 50.0))
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== GRID SIZING TEST OUTPUT ===")
        (message "%s" ascii-output)
        (message "==============================")
        
        ;; All three nodes should be visible in the output
        (expect ascii-output :to-match "Start")
        (expect ascii-output :to-match "Middle Blocker")
        (expect ascii-output :to-match "End") ; This should FAIL until we fix the grid sizing
        ))))

(provide 'dag-draw-grid-sizing-test)

;;; dag-draw-grid-sizing-test.el ends here