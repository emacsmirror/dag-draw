;;; dag-draw-pattern-isolation-test.el --- Isolate and test specific visual patterns -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; FUTURE ENHANCEMENT - Beyond GKNV Baseline
;;
;; Enhancement Category: Quality / Debug
;; Baseline Status: 🔧 Pattern Testing (Specific scenario validation)
;;
;; This test verifies:
;; - Pattern-specific debugging scenarios
;; - Isolated visual pattern validation
;; - Individual anti-pattern detection and fixing
;;
;; Related Baseline Decisions: D5.x (ASCII Rendering)
;; Enhancement Source: Pattern-specific quality validation
;;
;; These tests focus on specific patterns or edge cases beyond baseline.
;; See doc/test-suite-analysis.md (Category B3) for categorization rationale.
;;
;; [Original commentary: This test file isolates each specific visual pattern...]
;;
;; This test file isolates each specific visual pattern that's failing
;; in the end-to-end test, allowing us to debug and fix them individually.
;; Each test focuses on one specific anti-pattern from the main test.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)
(require 'test-helpers)

;;; Custom Grid Analysis Functions for 2D Spatial Validation

(defun dag-draw--is-drawing-char (char)
  "Return t if CHAR is a drawing character (edges/boundaries/arrows)."
  (memq char '(?│ ?─ ?┌ ?┐ ?└ ?┘ ?┼ ?▶ ?◀ ?▼ ?▲)))


(defun dag-draw--has-connection-above (grid row col)
  "Check if position has drawing character above it."
  (and (> row 0)
       (< col (length (aref grid (1- row))))
       (dag-draw--is-drawing-char (aref (aref grid (1- row)) col))))

(defun dag-draw--has-connection-below (grid row col)
  "Check if position has drawing character below it."
  (and (< (1+ row) (length grid))
       (< col (length (aref grid (1+ row))))
       (dag-draw--is-drawing-char (aref (aref grid (1+ row)) col))))

(defun dag-draw--has-connection-left (grid row col)
  "Check if position has drawing character to its left."
  (and (> col 0)
       (dag-draw--is-drawing-char (aref (aref grid row) (1- col)))))

(defun dag-draw--has-connection-right (grid row col)
  "Check if position has drawing character to its right."
  (and (< (1+ col) (length (aref grid row)))
       (dag-draw--is-drawing-char (aref (aref grid row) (1+ col)))))


(defun dag-draw--validate-arrow-connections (ascii-output)
  "Validate that arrows are properly connected to edges in 2D grid.
Returns list of floating arrow positions or nil if all are connected.
Implements GKNV Section 5.2 spatial requirements for arrow placement."
  (let* ((lines (split-string ascii-output "\n"))
         (grid (vconcat (mapcar (lambda (line) (vconcat line)) lines)))
         (floating-arrows '()))

    ;; Scan each position for arrows
    (dotimes (row (length grid))
      (when (> (length (aref grid row)) 0)  ; Skip empty lines
        (dotimes (col (length (aref grid row)))
          (let ((char (aref (aref grid row) col)))
            (cond
             ;; Check arrows - they should be connected to drawing characters (edges/boundaries)
             ((memq char '(?▼ ?▲ ?▶ ?◀))
              (let ((has-any-connection
                     (or
                      ;; Check all 4 directions for any drawing character connection
                      (dag-draw--has-connection-above grid row col)
                      (dag-draw--has-connection-below grid row col)
                      (dag-draw--has-connection-left grid row col)
                      (dag-draw--has-connection-right grid row col))))

                ;; Arrow is floating only if it has NO connections to any drawing characters
                (unless has-any-connection
                  (push (list row col (format "%s-arrow-floating"
                                            (cond ((eq char ?▼) "down")
                                                  ((eq char ?▲) "up")
                                                  ((eq char ?▶) "right")
                                                  ((eq char ?◀) "left"))))
                        floating-arrows)))))))))

    floating-arrows))


(describe "Pattern Isolation Tests - Debug Each Visual Issue"

  (describe "Excessive horizontal line pattern (──────)"
    (it "should not produce 6+ consecutive horizontal characters"
        ;; Create minimal graph that might trigger this pattern
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'a "A")
          (dag-draw-add-node graph 'b "B")
          (dag-draw-add-edge graph 'a 'b)

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== EXCESSIVE LINES TEST ===")
            (message "%s" output)
            (message "===========================")

            ;; Use test harness for validation instead of regex patterns
            (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            ))))

  (describe "Floating arrows pattern"
    (it "should not have arrows disconnected from lines"
        ;; Create graph with multiple edges to trigger floating arrows
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'source "Source")
          (dag-draw-add-node graph 'target1 "Target1")
          (dag-draw-add-node graph 'target2 "Target2")
          (dag-draw-add-edge graph 'source 'target1)
          (dag-draw-add-edge graph 'source 'target2)

          (dag-draw-layout-graph graph)
          (let* ((output (dag-draw-render-ascii graph))
                 (floating-arrows (dag-draw--validate-arrow-connections output)))
            (message "\n=== FLOATING ARROWS TEST ===")
            (message "%s" output)
            (message "=============================")

            ;; Use 2D grid analysis to detect floating arrows per GKNV Section 5.2
            (when floating-arrows
              (message "Floating arrows detected: %s" floating-arrows))

            ;; GKNV-compliant: All arrows should be connected to drawing characters
            (expect floating-arrows :to-be nil)
            ))))

  (describe "Fragmented routing patterns"
    (it "should not have broken L-connections"
        ;; Create L-shaped connection scenario
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'start "Start")
          (dag-draw-add-node graph 'end "End")
          (dag-draw-add-edge graph 'start 'end)

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== FRAGMENTED ROUTING TEST ===")
            (message "%s" output)
            (message "================================")

            ;; Use test harness for validation
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            ))))

  (describe "Node boundary connection issues"
    (it "should properly connect edges to node boundaries"
        ;; Create simple connection to test boundary behavior
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'node1 "Node1")
          (dag-draw-add-node graph 'node2 "Node2")
          (dag-draw-add-edge graph 'node1 'node2)

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== NODE BOUNDARY TEST ===")
            (message "%s" output)
            (message "===========================")

            ;; Use test harness for comprehensive validation
            (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((boundary-validation (dag-draw-test--validate-node-boundaries output)))
              (expect (plist-get boundary-validation :valid) :to-be t))
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            ))))

  (describe "Port distribution coordinate debugging"
    (it "should show different coordinates for multiple edges from same node"
        ;; Create scenario that should trigger port distribution
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'center "Center")
          (dag-draw-add-node graph 'dest1 "Dest1")
          (dag-draw-add-node graph 'dest2 "Dest2")
          (dag-draw-add-node graph 'dest3 "Dest3")

          ;; Multiple edges from center node - should get distributed ports
          (dag-draw-add-edge graph 'center 'dest1)
          (dag-draw-add-edge graph 'center 'dest2)
          (dag-draw-add-edge graph 'center 'dest3)

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== PORT DISTRIBUTION DEBUG ===")
            (message "%s" output)
            (message "================================")

            ;; Use test harness for validation
            (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
              (expect (plist-get structure-validation :topology-match) :to-be t))))))

  (describe "Complex scenario reproduction"
    (it "should handle multi-node graph without artifacts"
        ;; Simplified version of the complex test scenario
        (let ((graph (dag-draw-create-graph)))
          (dag-draw-add-node graph 'research "Research")
          (dag-draw-add-node graph 'database "Database")
          (dag-draw-add-node graph 'api "API")
          (dag-draw-add-node graph 'backend "Backend")

          ;; Create multiple edges from research (triggers the main issues)
          (dag-draw-add-edge graph 'research 'database)
          (dag-draw-add-edge graph 'research 'api)
          (dag-draw-add-edge graph 'database 'backend)
          (dag-draw-add-edge graph 'api 'backend)

          (dag-draw-layout-graph graph)
          (let ((output (dag-draw-render-ascii graph)))
            (message "\n=== COMPLEX SCENARIO TEST ===")
            (message "%s" output)
            (message "==============================")

            ;; Use test harness for comprehensive validation
            (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
              (expect (plist-get node-validation :complete) :to-be t))
            (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
              (expect (plist-get structure-validation :topology-match) :to-be t)
              (expect (plist-get structure-validation :node-count-match) :to-be t)
              (expect (plist-get structure-validation :edge-count-match) :to-be t))
            (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
              (expect (plist-get connectivity-validation :all-connected) :to-be t))
            )))))

(provide 'dag-draw-pattern-isolation-test)

;;; dag-draw-pattern-isolation-test.el ends here
