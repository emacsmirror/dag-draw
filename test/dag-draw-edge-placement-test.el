;;; dag-draw-edge-placement-test.el --- Tests for proper edge character placement -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Iteration 11: Fix edge character placement in complex graphs.
;; The issue is corner characters appearing inside node content instead of outside.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

(describe "dag-draw edge character placement"
  (describe "complex graph edge placement"
    (it "should not place corner characters inside node content"
      (let ((graph (dag-draw-create-graph)))
        ;; Create the exact scenario from the demo that causes the issue
        (dag-draw-add-node graph 'research "Research Phase")
        (dag-draw-add-node graph 'db-design "Database Design") 
        (dag-draw-add-node graph 'api-design "API Design")
        (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
        (dag-draw-add-node graph 'backend "Backend Implementation")
        (dag-draw-add-node graph 'frontend "Frontend Implementation")
        (dag-draw-add-node graph 'integration-test "Integration Testing")
        (dag-draw-add-node graph 'deployment "Deployment")
        
        ;; Add the problematic edges
        (dag-draw-add-edge graph 'research 'db-design)
        (dag-draw-add-edge graph 'research 'api-design)     
        (dag-draw-add-edge graph 'research 'infrastructure)
        (dag-draw-add-edge graph 'db-design 'backend)
        (dag-draw-add-edge graph 'api-design 'backend)
        (dag-draw-add-edge graph 'api-design 'frontend)
        (dag-draw-add-edge graph 'infrastructure 'frontend)  
        (dag-draw-add-edge graph 'backend 'integration-test)
        (dag-draw-add-edge graph 'frontend 'integration-test)
        (dag-draw-add-edge graph 'integration-test 'deployment)
        
        (dag-draw-layout-graph graph)
        
        (let ((result (dag-draw-render-ascii graph)))
          ;; Node content should remain intact - check for parts since they appear on separate lines
          (expect result :to-match "Backend")
          (expect result :to-match "Implementation")
          (expect result :to-match "Frontend")
          
          ;; Should NOT have corner characters corrupting node content - this is the key fix
          (expect result :not :to-match "Backend Implementatio└")
          (expect result :not :to-match "Frontend Imple└entation")
          (expect result :not :to-match "Implementation└")
          
          ;; Should still have proper edge routing with corners outside nodes
          (expect result :to-match "[└┌┐┘]")  ; Has corner characters
          (expect result :to-match "[▶▼◀▲]"))))) ; Has arrow characters

  (describe "node boundary protection"
    (it "should maintain clear separation between node content and edge characters"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'A "LongNodeName")
        (dag-draw-add-node graph 'B "AnotherNode")
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)
        
        (let ((result (dag-draw-render-ascii graph)))
          ;; Node names should be completely intact
          (expect result :to-match "LongNodeName")
          (expect result :to-match "AnotherNode")
          
          ;; No edge characters should appear within node boundaries
          (expect result :not :to-match "Long└NodeName")
          (expect result :not :to-match "Another└Node")
          (expect result :not :to-match "LongNode└Name"))))))

(provide 'dag-draw-edge-placement-test)

;;; dag-draw-edge-placement-test.el ends here