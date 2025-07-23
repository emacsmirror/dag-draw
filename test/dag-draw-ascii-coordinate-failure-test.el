;;; dag-draw-ascii-coordinate-failure-test.el --- Test showing ASCII coordinate collapse -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; This test captures the exact failure shown by the user:
;; ASCII coordinate mode produces trampled nodes where ranks collapse into each other.
;; This violates GKNV aesthetic A2 (avoid visual anomalies).

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "ASCII coordinate system fundamental failures"
  
  (it "should prevent node trampling in complex dependency graph"
    (let ((graph (dag-draw-create-graph)))
      ;; Create the exact graph showing trampling
      (dag-draw-add-node graph 'research "Research")
      (dag-draw-add-node graph 'database "Database Design")
      (dag-draw-add-node graph 'api "API Design")
      (dag-draw-add-node graph 'infrastructure "Infrastructure Setup")
      (dag-draw-add-node graph 'backend "Backend Implementation")
      (dag-draw-add-node graph 'frontend "Frontend Implementation")
      (dag-draw-add-node graph 'integration "Integration Testing")
      (dag-draw-add-node graph 'deployment "Deployment")

      ;; Add dependencies
      (dag-draw-add-edge graph 'research 'database)
      (dag-draw-add-edge graph 'research 'api)
      (dag-draw-add-edge graph 'research 'infrastructure)
      (dag-draw-add-edge graph 'database 'backend)
      (dag-draw-add-edge graph 'api 'backend)
      (dag-draw-add-edge graph 'api 'frontend)
      (dag-draw-add-edge graph 'backend 'integration)
      (dag-draw-add-edge graph 'frontend 'integration)
      (dag-draw-add-edge graph 'integration 'deployment)

      ;; Layout with ASCII coordinates (the broken mode)
      (dag-draw-layout-graph graph :coordinate-mode 'ascii)
      
      ;; Get ASCII output
      (let ((output (dag-draw-render-ascii graph)))
        (message "ASCII OUTPUT SHOWING TRAMPLING:")
        (message "%s" output)
        
        ;; SUCCESS VALIDATION
        ;; These checks should all PASS with proper ASCII-native coordinates
        
        ;; 1. No text corruption patterns (letters cut off by node boundaries)
        ;; Bad: "│Backend I│p│e│en│ation │" - text fragmented by overlapping boundaries
        (expect output :not :to-match "[a-zA-Z]│[a-zA-Z]│")  ; Text-boundary-text-boundary indicates overlap
        
        ;; 2. No partial node text corruption  
        ;; Text like "n│" indicates nodes drawn on top of each other
        (expect output :not :to-match "[a-zA-Z][a-zA-Z]│")  ; Multiple letters then boundary (truncation)
        
        ;; 3. Each node should have complete, readable text
        (expect output :to-match "Research")
        (expect output :to-match "Database Design")  
        (expect output :to-match "API Design")
        (expect output :to-match "Infrastructure Setup")
        (expect output :to-match "Backend Implementation")
        (expect output :to-match "Frontend Implementation")
        (expect output :to-match "Integration Testing")
        (expect output :to-match "Deployment")
        
        ;; 4. Nodes should be separated vertically (different Y coordinates)
        ;; Count the number of distinct horizontal lines that contain node text
        (let ((lines (split-string output "\n"))
              (node-lines 0))
          (dolist (line lines)
            (when (and (string-match-p "┌\\|└\\|│" line)  ; Contains node boundary chars
                       (string-match-p "[A-Za-z]" line))   ; Contains actual text
              (setq node-lines (1+ node-lines))))
          
          ;; Should have multiple distinct lines with nodes, not all crammed together
          (expect node-lines :to-be-greater-than 4))))))

;;; dag-draw-ascii-coordinate-failure-test.el ends here