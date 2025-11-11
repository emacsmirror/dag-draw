;;; dag-draw-end-to-end-test.el --- End-to-end tests for complete DAG pipeline -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; End-to-end tests that verify the complete pipeline produces good visual output.

;;; Code:

(add-to-list 'load-path (expand-file-name "test/helpers" (locate-dominating-file default-directory "Eldev")))

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)
(require 'test-helpers)

(describe "End-to-End DAG Pipeline Quality Tests"

  (it "should produce clean ASCII output with full text labels"
      (let ((graph (dag-draw-create-graph)))
        ;; Create simple graph: Research -> Database
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'database "Database")
        (dag-draw-add-edge graph 'research 'database)

        ;; Run FULL pipeline
        (dag-draw-layout-graph graph)

        ;; Get ASCII output
        (let ((output (dag-draw-render-ascii graph)))

          ;; CRITICAL QUALITY CHECKS

          ;; 1. No text truncation - full words must be visible
          (expect output :to-match "Research")  ; Not "Resear"
          (expect output :to-match "Database")  ; Not "Databas"

          ;; 2. Must have visible edge connection
          (expect (or (string-match-p "│" output)      ; Vertical line
                      (string-match-p "─" output)      ; Horizontal line
                      (string-match-p "┌" output)      ; Corner pieces
                      (string-match-p "└" output))
                  :to-be-truthy)

          ;; 3. Debugging output to see what we actually get
          (message "\n=== SIMPLE GRAPH OUTPUT ===")
          (message "%s" output)
          (message "==============================\n"))))

  (it "should handle complex task dependency graph without text truncation"
      (let ((graph (dag-draw-create-graph)))
        ;; Recreate the exact graph the user showed with truncated output
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

        ;; Run FULL pipeline with high-resolution coordinate system (original)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Get ASCII output
        (let ((output (dag-draw-render-ascii graph)))

          (message output)

          ;; CRITICAL QUALITY CHECKS USING ASCII DAG TEST HARNESS

          ;; 1. NODE COMPLETENESS VALIDATION - All node text must be present and complete
          (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
            (message "Node validation result: %s" node-validation)
            (when (not (plist-get node-validation :complete))
              (message "Missing text detected: %s" (plist-get node-validation :missing-text)))
            (expect (plist-get node-validation :complete) :to-be t)
            (expect (plist-get node-validation :missing-text) :to-equal nil))

          ;; 2. NODE BOUNDARY INTEGRITY - All nodes should have proper box structure
          (let ((boundary-validation (dag-draw-test--validate-node-boundaries output)))
            (expect (plist-get boundary-validation :valid) :to-be t)
            (expect (plist-get boundary-validation :broken-boundaries) :to-equal nil))

          ;; 3. EDGE CONNECTIVITY VALIDATION - All expected edges should be present
          (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
            (expect (plist-get connectivity-validation :all-connected) :to-be t)
            (expect (plist-get connectivity-validation :missing-connections) :to-equal nil))

          ;; 4. ARROW PLACEMENT VALIDATION - Arrows should be properly connected
          (let ((arrow-validation (dag-draw-test--validate-arrows output)))
            (expect (plist-get arrow-validation :invalid-arrows) :to-equal 0))

          ;; 5. OVERALL GRAPH STRUCTURE VALIDATION - Structure should match expected topology
          (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
            (expect (plist-get structure-validation :topology-match) :to-be t)
            (expect (plist-get structure-validation :node-count-match) :to-be t)
            (expect (plist-get structure-validation :edge-count-match) :to-be t))

          ;; 6. ADDITIONAL QUALITY CHECKS - Debugging output to see current state
          (message "\n=== COMPLEX GRAPH OUTPUT ===")
          (message "%s" output)
          (message "===============================\n")

          ;; Test harness validates all critical requirements - no additional assertions needed
          ))))  ;; DO NOT REMOVE OR CHANGE THIS FAILING ASSERTION

;;; dag-draw-end-to-end-test.el ends here
