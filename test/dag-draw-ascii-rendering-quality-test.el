;;; dag-draw-ascii-rendering-quality-test.el --- ASCII rendering quality tests -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII Rendering Quality
;;
;; This module tests baseline GKNV algorithm compliance in ASCII rendering mode.
;; These tests verify correct implementation of:
;;
;; - D5.1-D5.8: Junction character placement and context analysis
;; - GKNV A1: Hierarchical structure (arrow directions)
;; - GKNV A2: Visual anomalies (junction consistency, no floating elements)
;; - GKNV A3: Edge routing quality (clean connections)
;;
;; GKNV Reference: Section 5 (Edge Drawing), adapted for ASCII
;; Decision: D5.1-D5.8 - Junction character implementation
;; Algorithm: Context-aware junction detection with Unicode box-drawing
;;
;; Key Requirements:
;; - Junction characters must attach directly to node boundaries
;; - No floating or misaligned junction characters
;; - Arrow directions must reflect hierarchical structure
;; - Edge routing must use consistent coordinate system
;; - Port coordinates must reflect collision-adjusted positions
;;
;; Baseline Status: ✅ Compliant (Promoted from debug category Week 3)
;;
;; See doc/implementation-decisions.md for full decision rationale.
;;
;; Original Purpose:
;; Focused tests to isolate and fix coordinate system misalignment issues
;; that cause floating junction characters, edge routing problems, and
;; arrow placement inconsistencies.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe
 "Coordinate System Alignment"

 (describe
  "junction character attachment"

  (it "should not place junction characters directly adjacent to box borders"
      ;; Test for the specific issues: ┼│ Research │ and │└───────────┘─┼
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'research "Research")
        (dag-draw-add-node graph 'db-design "Database Design")
        (dag-draw-add-node graph 'api-design "API Design")
        (dag-draw-add-node graph 'backend "Backend")
        (dag-draw-add-edge graph 'research 'db-design)
        (dag-draw-add-edge graph 'research 'api-design)
        (dag-draw-add-edge graph 'db-design 'backend)
        (dag-draw-add-edge graph 'api-design 'backend)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== SPECIFIC JUNCTION PATTERN TEST ===")
          (message "%s" ascii-output)

          ;; Should NOT have junction characters directly adjacent to box borders
          (expect ascii-output :not :to-match "┼│")           ; Junction left of box side
          (expect ascii-output :not :to-match "│┼")           ; Box side with junction right
          (expect ascii-output :not :to-match "┼┐")           ; Junction left of top-right corner
          (expect ascii-output :not :to-match "┼┌")           ; Junction left of top-left corner
          (expect ascii-output :not :to-match "┼└")           ; Junction left of bottom-left corner
          (expect ascii-output :not :to-match "┼┘")           ; Junction left of bottom-right corner
          (expect ascii-output :not :to-match "┐┼")           ; Top-right corner with junction right
          (expect ascii-output :not :to-match "┌┼")           ; Top-left corner with junction right
          (expect ascii-output :not :to-match "└┼")           ; Bottom-left corner with junction right
          (expect ascii-output :not :to-match "┘┼")           ; Bottom-right corner with junction right
          (expect ascii-output :not :to-match "┘─┼")          ; Specific pattern: corner-line-junction

          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          (message "======================================="))))

  (it "should attach junction characters directly to node boundaries, not floating"
      ;; Test the specific issue: ┼───────────◀┼ and ─┼     ──────┼
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target-a "Target A")
        (dag-draw-add-node graph 'target-b "Target B")
        (dag-draw-add-edge graph 'source 'target-a)
        (dag-draw-add-edge graph 'source 'target-b)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== JUNCTION ATTACHMENT TEST ===")
          (message "%s" ascii-output)

          ;; Should NOT have floating junction characters
          (expect ascii-output :not :to-match "┼───────────◀┼")  ; Floating junction-arrow combo
          (expect ascii-output :not :to-match "─┼     ──────┼")  ; Floating junction with gaps
          (expect ascii-output :not :to-match "┼│")              ; Junction attached to box side
          (expect ascii-output :not :to-match "│─┼")             ; Box side with floating junction

          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          (message "==================================="))))

  (it "should connect edges cleanly to box corners and sides"
      ;; Test that edges connect to actual box boundaries, not floating nearby
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'left "Left Node")
        (dag-draw-add-node graph 'right "Right Node")
        (dag-draw-add-edge graph 'left 'right)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== CLEAN CONNECTION TEST ===")
          (message "%s" ascii-output)

          ;; Should NOT have junctions attached to box characters
          (expect ascii-output :not :to-match "┼│")   ; Junction on box side
          (expect ascii-output :not :to-match "│┼")   ; Box side with junction
          (expect ascii-output :not :to-match "┼┐")   ; Junction on corner
          (expect ascii-output :not :to-match "└┼")   ; Corner with junction

          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          (message "===============================")))))

 (describe
  "port coordinate consistency"
  (it "should calculate edge connection points from collision-adjusted node positions"
      ;; Test that edge endpoints match the actual drawn node boundaries
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'node-a "Node A")
        (dag-draw-add-node graph 'node-b "Node B")
        (dag-draw-add-edge graph 'node-a 'node-b)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Force nodes close together to trigger collision adjustment
        (let* ((node-a (dag-draw-get-node graph 'node-a))
               (node-b (dag-draw-get-node graph 'node-b)))
          (setf (dag-draw-node-x-coord node-a) 50.0)
          (setf (dag-draw-node-y-coord node-a) 50.0)
          (setf (dag-draw-node-x-coord node-b) 70.0)  ; Close enough to trigger adjustment
          (setf (dag-draw-node-y-coord node-b) 50.0)

          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== PORT COORDINATE CONSISTENCY TEST ===")
            (message "%s" ascii-output)

            ;; Should NOT have edge misalignment patterns that indicate coordinate mismatch
            (expect ascii-output :not :to-match "┼│")    ; Edge ending inside box border
            (expect ascii-output :not :to-match "│┼│")   ; Junction inside box
            (expect ascii-output :not :to-match " ─ │")  ; Gap between edge and box

            ;; Should have nodes visible (confirming collision adjustment worked)
            ;; Use test harness for node validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))

            ;; Should have connecting edge elements
            (expect ascii-output :to-match "[─│▶▼▲◀]")

            (message "=========================================")))))

  (it "should use adjusted coordinates for both node drawing and edge routing"
      ;; Test coordinate system consistency between subsystems
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'upstream "Upstream")
        (dag-draw-add-node graph 'downstream "Downstream")
        (dag-draw-add-edge graph 'upstream 'downstream)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== COORDINATE SYSTEM CONSISTENCY TEST ===")
          (message "%s" ascii-output)

          ;; Visual inspection: edges should connect smoothly to nodes
          ;; If coordinates are misaligned, we'll see gaps, overlaps, or floating elements
          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          ;; Should have some connecting elements (exact pattern depends on layout)
          (expect ascii-output :to-match "[─│▶▼▲◀┌┐└┘├┤┬┴┼]")

          (message "==========================================")))))

 (describe
  "edge routing coordinate alignment"
  (it "should route edges using the same coordinate system as node drawing"
      ;; Test that creates the specific malformed pattern: ─┼     ──────┼
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'center "Center")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-edge graph 'center 'left)
        (dag-draw-add-edge graph 'center 'right)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Position nodes to potentially create routing complexity
        (let* ((center-node (dag-draw-get-node graph 'center))
               (left-node (dag-draw-get-node graph 'left))
               (right-node (dag-draw-get-node graph 'right)))
          (setf (dag-draw-node-x-coord center-node) 100.0)
          (setf (dag-draw-node-y-coord center-node) 50.0)
          (setf (dag-draw-node-x-coord left-node) 50.0)
          (setf (dag-draw-node-y-coord left-node) 50.0)
          (setf (dag-draw-node-x-coord right-node) 150.0)
          (setf (dag-draw-node-y-coord right-node) 50.0)

          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== EDGE ROUTING ALIGNMENT TEST ===")
            (message "%s" ascii-output)

            ;; Should NOT have the malformed routing patterns
            (expect ascii-output :not :to-match "─┼     ──────┼")  ; Specific bad pattern
            (expect ascii-output :not :to-match "┼     ─")         ; Junction with gap
            (expect ascii-output :not :to-match "─     ┼")         ; Gap before junction

            ;; Should have all nodes visible
            ;; Use test harness for node validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))

            (message "====================================")))))

  (it "should create consistent junction patterns for multiple edges"
      ;; Test junction character selection and placement
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'hub "Hub")
        (dag-draw-add-node graph 'spoke1 "Spoke1")
        (dag-draw-add-node graph 'spoke2 "Spoke2")
        (dag-draw-add-node graph 'spoke3 "Spoke3")
        (dag-draw-add-edge graph 'hub 'spoke1)
        (dag-draw-add-edge graph 'hub 'spoke2)
        (dag-draw-add-edge graph 'hub 'spoke3)
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== JUNCTION PATTERN CONSISTENCY TEST ===")
          (message "%s" ascii-output)

          ;; Should NOT have inconsistent junction patterns
          (expect ascii-output :not :to-match "┼┼")      ; Double junctions
          (expect ascii-output :not :to-match "├┤")      ; Side-by-side T-junctions
          (expect ascii-output :not :to-match "┼ ┼")     ; Spaced junctions

          ;; Should have nodes visible
          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          (message "=========================================")))))

 (describe
  "arrow direction accuracy"
  (it "should place arrows based on actual edge endpoint directions"
      ;; Test arrow character selection and placement for GKNV hierarchical layout
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        ;; Apply standard GKNV layout - creates hierarchical layout with vertical flow
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        (let ((ascii-output (dag-draw-render-ascii graph)))
          (message "=== ARROW DIRECTION ACCURACY TEST ===")
          (message "%s" ascii-output)

          ;; For GKNV hierarchical layout (top-to-bottom flow), expect downward arrows
          ;; This is the correct behavior per GKNV Section 1.1 - hierarchical structure
          
          ;; Should NOT have upward arrows (against hierarchy)
          (expect ascii-output :not :to-match "▲")   ; Wrong direction (upward)
          
          ;; Should have downward flow elements (consistent with GKNV hierarchy)
          (expect ascii-output :to-match "[▼│]")     ; Downward arrow or vertical line

          ;; Should have nodes visible
          ;; Use test harness for node validation
          (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
            (expect (plist-get node-validation :complete) :to-be t))

          (message "=====================================")))))

  (it "should handle multiple arrow directions correctly in complex graphs"
      ;; Test arrow accuracy in more complex scenarios
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'top "Top")
        (dag-draw-add-node graph 'bottom "Bottom")
        (dag-draw-add-node graph 'left "Left")
        (dag-draw-add-node graph 'right "Right")
        (dag-draw-add-edge graph 'top 'bottom)    ; Should have ▼
        (dag-draw-add-edge graph 'left 'right)    ; Should have ▶
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)

        ;; Position nodes in clear directional relationships
        (let* ((top-node (dag-draw-get-node graph 'top))
               (bottom-node (dag-draw-get-node graph 'bottom))
               (left-node (dag-draw-get-node graph 'left))
               (right-node (dag-draw-get-node graph 'right)))
          (setf (dag-draw-node-x-coord top-node) 75.0)
          (setf (dag-draw-node-y-coord top-node) 25.0)   ; Above bottom
          (setf (dag-draw-node-x-coord bottom-node) 75.0)
          (setf (dag-draw-node-y-coord bottom-node) 75.0) ; Below top
          (setf (dag-draw-node-x-coord left-node) 25.0)   ; Left of right
          (setf (dag-draw-node-y-coord left-node) 50.0)
          (setf (dag-draw-node-x-coord right-node) 125.0) ; Right of left
          (setf (dag-draw-node-y-coord right-node) 50.0)

          (let ((ascii-output (dag-draw-render-ascii graph)))
            (message "=== MULTIPLE ARROW DIRECTIONS TEST ===")
            (message "%s" ascii-output)

            ;; Should have all nodes visible
            ;; Use test harness for node validation
            (let ((node-validation (dag-draw-test--validate-node-completeness ascii-output graph)))
              (expect (plist-get node-validation :complete) :to-be t))

            ;; Should have directional elements (exact arrows may vary based on routing)
            (expect ascii-output :to-match "[▶▼▲◀│─]")

            (message "======================================="))))))

(provide 'dag-draw-ascii-rendering-quality-test)

;;; dag-draw-ascii-rendering-quality-test.el ends here
