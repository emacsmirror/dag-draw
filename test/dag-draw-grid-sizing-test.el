;;; dag-draw-grid-sizing-test.el --- Test grid sizing issues -*- lexical-binding: t -*-

;; Test to reproduce the issue where nodes are positioned outside grid boundaries

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Grid Sizing Issues"
  (it "should include all nodes within grid boundaries after spline regeneration"
    ;; Test case that reproduces the missing "End" node issue
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle Blocker") 
      (dag-draw-add-node graph 'end "End")
      
      ;; Position nodes horizontally to trigger the coordinate issue
      (dag-draw-add-edge graph 'start 'end)
      (dag-draw-layout-graph graph :coordinate-mode 'ascii)
      
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
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        ))))

(provide 'dag-draw-grid-sizing-test)

;;; dag-draw-grid-sizing-test.el ends here