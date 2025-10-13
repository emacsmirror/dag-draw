;;; dag-draw-junction-test.el --- Test junction character issues -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Junction Character Issues"
  (it "should not create floating junction characters"
    ;; Test the specific issue: ┼───────────◀┼ and ─┼     ──────┼
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'source "Source")
      (dag-draw-add-node graph 'target-a "Target A")  
      (dag-draw-add-node graph 'target-b "Target B")
      (dag-draw-add-edge graph 'source 'target-a)
      (dag-draw-add-edge graph 'source 'target-b)
      (dag-draw-layout-graph graph :coordinate-mode 'ascii)
      
      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== JUNCTION TEST ===")
        (message "%s" ascii-output)
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
          (expect (plist-get connectivity-validation :all-connected) :to-be t))
        
        (message "==================")))))

(provide 'dag-draw-junction-test)