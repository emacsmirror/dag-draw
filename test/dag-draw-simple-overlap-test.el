;;; dag-draw-simple-overlap-test.el --- Simple test for node overlap defect -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-render)

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
        
        ;; Test for actual text visibility - all node names should be fully visible
        (expect output :to-match "API Design")  ; Should not be partially hidden
        (expect output :to-match "Infrastructure Setup")  ; Should not hide other nodes
        
        ;; Look for the specific problematic pattern that indicates merged nodes
        ;; The issue is when we get something like: │Database Design  │     │API Design│Infrastructure Setup  │
        ;; This suggests nodes are being drawn as one continuous structure
        (expect output :not :to-match "│[^│]*│[^│]*│[^│]*│[^│]*│")  ; More than 4 separators in one line = merged structure
        
        ;; Test that we don't have the "│──" pattern which indicates visual ambiguity
        (expect output :not :to-match "│──")))))

(provide 'dag-draw-simple-overlap-test)

;;; dag-draw-simple-overlap-test.el ends here