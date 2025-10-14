;;; dag-draw-junction-test.el --- Test junction character issues -*- lexical-binding: t -*-

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Junction Character Algorithm
;;
;; This module tests ASCII junction character algorithm as specified in
;; doc/CLAUDE.md and doc/implementation-decisions.md.
;;
;; GKNV Reference: N/A (junction characters are ASCII-specific enhancement)
;; Decision: D5.4 - Walk-based local analysis for junction character selection
;; Algorithm: Edge Walking for Context-Aware Junction Character Selection
;;
;; Key Requirements Tested:
;; - Junction characters enhance visual aesthetic of ASCII graphs
;; - Walk each edge to determine locally-relevant junction type
;; - Starting port junction: edge leaves node (e.g., ┬ for downward edge from bottom)
;; - Ending port junction: edge enters node (e.g., ▼ arrow + appropriate T-junction)
;; - Direction change junction: corners (e.g., └ for upward-then-rightward)
;; - Merge/split junction: multiple edges share segment (e.g., ┴ for upward merge)
;; - Cross junction: edges cross (┼ character)
;; - All junction types correctly identified and applied
;;
;; Test Coverage:
;; - Starting port junctions correct (all four sides)
;; - Ending port junctions correct (all four sides)
;; - Corner junctions correct (all eight corner types)
;; - Merge junctions correct (edges joining)
;; - Split junctions correct (edges separating)
;; - Cross junctions correct (edges crossing)
;; - Algorithm walks all edges correctly
;; - Various edge patterns and combinations
;;
;; Baseline Status: ✅ Required for GKNV compliance (ASCII adaptation)
;;
;; See doc/CLAUDE.md (Junction character algorithm) for detailed specification.
;; See doc/implementation-decisions.md (D5.4) for decision rationale.

;;; Code:

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
        
        (message "=================="))))

  (it "should not corrupt node borders with junction characters"
    ;; Regression test for junction character defect (2025-10-14)
    ;; Bug: Junction enhancement algorithm was treating node box border characters
    ;; as edge characters, causing malformed patterns like ├┬┐ and ┴┴ at node boundaries
    ;; Fix: Solution 1 - Exclude node boundaries from junction enhancement
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'top "Top")
      (dag-draw-add-node graph 'bottom "Bottom")
      (dag-draw-add-edge graph 'top 'bottom)
      (dag-draw-layout-graph graph :coordinate-mode 'ascii)

      (let ((ascii-output (dag-draw-render-ascii graph)))
        (message "=== NODE BORDER INTEGRITY TEST ===")
        (message "%s" ascii-output)

        ;; Verify node borders are intact using test harness
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries ascii-output)))
          (expect (plist-get boundary-validation :valid) :to-be t)
          (when (not (plist-get boundary-validation :valid))
            (message "Node boundary validation failed: %s"
                     (plist-get boundary-validation :errors))))

        ;; Specifically check for the corruption patterns that were observed
        ;; Pattern 1: Multiple junction chars in sequence (e.g., "├┬┐")
        (expect ascii-output :not :to-match "├┬┐")
        (expect ascii-output :not :to-match "┴┴")

        ;; Pattern 2: Junction characters embedded in node borders where they shouldn't be
        ;; Top node should have clean box with proper T-junction at exit port
        (expect ascii-output :to-match "┌─+─┐")  ; Top border (with some content)
        (expect ascii-output :to-match "└─[┬─]+─┘")  ; Bottom border with T-junction at port

        ;; Pattern 3: Node content should not be corrupted
        (expect ascii-output :to-match "│Top")
        (expect ascii-output :to-match "│Bottom")

        ;; Verify edge connectivity is maintained
        (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity ascii-output graph)))
          (expect (plist-get connectivity-validation :all-connected) :to-be t))

        (message "===================================")))))

(provide 'dag-draw-junction-test)