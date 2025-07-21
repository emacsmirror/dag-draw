;;; dag-draw-simple-overlap-test.el --- Simple test for node overlap defect -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Simple Node Overlap Test"

  (it "should not have overlapping nodes that hide text"
    (let ((graph (dag-draw-create-graph)))
      
      ;; Recreate the exact problematic case from the complex test
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")

      ;; Add the dependency relationships
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'research 'infrastructure)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'api 'frontend)
      (dag-draw-add-edge graph 'backend 'integration)
      (dag-draw-add-edge graph 'frontend 'integration)
      (dag-draw-add-edge graph 'integration 'deployment)
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Get output
      (let ((output (dag-draw-render-ascii graph)))
        (message "\n=== COMPLEX OVERLAP TEST ===")
        (message "%s" output)
        (message "=============================\n")
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
          (expect (plist-get structure-validation :topology-match) :to-be t)
          (expect (plist-get structure-validation :node-count-match) :to-be t))
        (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
          (expect (plist-get connectivity-validation :all-connected) :to-be t))))))

(provide 'dag-draw-simple-overlap-test)

;;; dag-draw-simple-overlap-test.el ends here