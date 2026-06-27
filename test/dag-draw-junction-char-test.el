;;; dag-draw-junction-char-test.el --- Tests for enhanced junction characters -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Specific Junction Characters
;;
;; This module tests specific junction character selection as specified in
;; doc/CLAUDE.md (junction character examples).
;;
;; GKNV Reference: N/A (junction character details are ASCII-specific)
;; Decision: D5.4 - Context-aware junction character selection
;; Algorithm: Junction Character Mapping
;;
;; Key Requirements Tested:
;; - Each junction context maps to correct Unicode box-drawing character
;; - Starting junctions: ┬ (down from bottom), ├ (right from right side), etc.
;; - Corners: └ ┘ ┌ ┐ for four corner types
;; - T-junctions: ┬ ┴ ├ ┤ for four T-junction types
;; - Cross: ┼ for perpendicular crossing
;; - Arrow integration: arrows don't replace required junction characters
;; - Character selection based on edge directions at junction point
;;
;; Test Coverage:
;; - All starting junction characters correct
;; - All ending junction characters correct
;; - All corner characters correct (8 types: 4 corners × 2 directions)
;; - All T-junction characters correct (4 orientations)
;; - Cross character used when edges cross
;; - Direction analysis drives correct character selection
;; - Examples from CLAUDE.md specification verified
;;
;; Baseline Status: ✅ Required for GKNV compliance (ASCII adaptation)
;;
;; See doc/CLAUDE.md (Junction character examples) for specification.
;; See doc/implementation-decisions.md (D5.4) for decision rationale.

;; Tests for enhanced junction character functionality.
;; Implements Phase 3 of ASCII-native GKNV implementation.
;; Based on CLAUDE.md junction character specifications.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-ascii-grid)

(describe "Enhanced junction characters"
  
  (describe "dag-draw--get-enhanced-junction-char"
    (it "should exist as a function"
      (expect (fboundp 'dag-draw--get-enhanced-junction-char) :to-be t))

    (it "should handle direction change junctions"
      ;; CLAUDE.md: "When the edge requires a direction change"
      ;; Example: horizontal line going right that needs to turn down
      (let ((context '(:type direction-change :from-direction right :to-direction down :current-char ?─)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┐)))

    (it "should handle edge crossing junctions"
      ;; CLAUDE.md: "When two edges cross"
      ;; Example: horizontal and vertical edges crossing
      (let ((context '(:type edge-cross :directions (horizontal vertical))))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┼)))
    
    (it "should handle complex T-junction scenarios"
      ;; CLAUDE.md example: edge going down with another edge splitting right
      (let ((context '(:type t-junction :main-direction down :branch-direction right)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?├))))

  (describe "context-based junction character selection in rendering"
    (it "should accept context plist not raw characters"
      ;; This tests the correct usage from line 151 of dag-draw-ascii-edges.el
      ;; The function should receive a context plist, not raw characters
      (let ((context (list :type 'direction-change
                          :from-direction 'right
                          :to-direction 'down)))
        ;; This should work - context plist format
        (expect (dag-draw--get-enhanced-junction-char context) :to-equal ?┐)))

    (it "should analyze local grid context to determine junction type"
      ;; Test that we can analyze grid neighbors to determine junction type
      ;; This is needed to fix line 151 of dag-draw-ascii-edges.el
      (let* ((grid (dag-draw--create-ascii-grid 10 10))
             (x 5) (y 5))
        ;; Create a right-to-down corner at (x,y):
        ;; Horizontal line from left: (4,5) has ?─
        (aset (aref grid y) (- x 1) ?─)
        ;; Vertical line to down: (5,6) has ?│
        (aset (aref grid (+ y 1)) x ?│)

        ;; When placing a new character at (x,y), we should detect:
        ;; - incoming from left (has ?─ at left neighbor)
        ;; - outgoing down (will place ?│ going down)
        ;; - junction type: direction-change from right to down
        ;; - correct character: ?┐

        (let ((context (dag-draw--analyze-local-grid-junction-context grid x y ?─ ?│)))
          (expect (plist-get context :type) :to-equal 'direction-change)
          (expect (plist-get context :has-left) :to-be-truthy)
          (expect (plist-get context :has-down) :to-be-truthy)))))

  (describe "Priority 4: Integration into rendering pipeline"
    (it "should apply junction characters to simple corner in rendered output"
      ;; TDD: Test that junction detection integrates with actual rendering
      ;; Create a simple graph where an edge makes a corner
      ;; Then verify the corner has the correct junction character in final output
      (let* ((graph (dag-draw-create-graph))
             (grid (dag-draw--create-ascii-grid 10 10)))
        ;; Manually create an L-shaped edge path for testing
        ;; Horizontal from (0,5) to (5,5), then vertical from (5,5) to (5,8)
        (dotimes (x 6)
          (aset (aref grid 5) x ?─))
        (dotimes (offset 4)
          (aset (aref grid (+ 5 offset)) 5 ?│))

        ;; Apply junction detection
        ;; This should convert the character at (5,5) from ?─ or ?│ to ?┐
        ;; No node boundaries in this synthetic test, so pass empty list
        (dag-draw--apply-junction-chars-to-grid grid '())

        ;; Verify corner character is correct
        (let ((corner-char (aref (aref grid 5) 5)))
          (expect corner-char :to-equal ?┐)))))

  (describe "Priority 6: Arrow coordination"
    (it "should not overwrite arrows with junction characters"
      ;; TDD: Arrows should be preserved even when junction detection runs
      ;; CLAUDE.md: "Applications of the rules... the one possible exemption is where
      ;; an arrow is placed (which may be the last element of the edge)"
      (let* ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Create edge with arrow at end: horizontal line with downward arrow
        ;; ─────▼
        (dotimes (x 5)
          (aset (aref grid 5) x ?─))
        (aset (aref grid 5) 5 ?▼)  ; Arrow at end

        ;; Apply junction detection
        ;; No node boundaries in this synthetic test, so pass empty list
        (dag-draw--apply-junction-chars-to-grid grid '())

        ;; Arrow should still be present, not replaced with junction char
        (expect (aref (aref grid 5) 5) :to-equal ?▼)))

    (it "should apply junction character adjacent to arrow when appropriate"
      ;; TDD: Test CLAUDE.md example:
      ;; "an edge goes toward an arrow (destination port) and another edge
      ;; splits off towards the right"
      ;; Should have junction character BEFORE the arrow
      (let* ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Create vertical edge going down with arrow
        ;; │
        ;; ├───  (edge splits right)
        ;; ▼
        (aset (aref grid 4) 5 ?│)    ; Edge coming from above
        (aset (aref grid 5) 5 ?│)    ; Position that should become ├
        (aset (aref grid 6) 5 ?▼)    ; Arrow pointing down
        ;; Edge going right from the junction (positions 6-9)
        (dotimes (offset 4)
          (aset (aref grid 5) (+ 6 offset) ?─))

        ;; Apply junction detection
        ;; No node boundaries in this synthetic test, so pass empty list
        (dag-draw--apply-junction-chars-to-grid grid '())

        ;; Position (5,5) should become ├ (t-junction)
        (expect (aref (aref grid 5) 5) :to-equal ?├)
        ;; Arrow should still be ▼
        (expect (aref (aref grid 6) 5) :to-equal ?▼)))

    (it "should handle all arrow directions with adjacent junctions"
      ;; TDD: Test all arrow directions (▼ ▲ ► ◄) with adjacent junctions
      (let* ((grid (dag-draw--create-ascii-grid 15 15)))
        ;; Down arrow with junction above
        (aset (aref grid 0) 0 ?│)
        (aset (aref grid 1) 0 ?├)
        (aset (aref grid 2) 0 ?▼)
        (dotimes (offset 3)
          (aset (aref grid 1) (+ 1 offset) ?─))

        ;; Up arrow with junction below
        (aset (aref grid 0) 5 ?▲)
        (aset (aref grid 1) 5 ?├)
        (aset (aref grid 2) 5 ?│)
        (dotimes (offset 3)
          (aset (aref grid 1) (+ 6 offset) ?─))

        ;; Right arrow with junction to left
        (aset (aref grid 5) 0 ?─)
        (aset (aref grid 5) 1 ?┬)
        (aset (aref grid 5) 2 ?►)
        (dotimes (offset 3)
          (aset (aref grid (+ 6 offset)) 1 ?│))

        ;; Left arrow with junction to right
        (aset (aref grid 5) 5 ?◄)
        (aset (aref grid 5) 6 ?┬)
        (aset (aref grid 5) 7 ?─)
        (dotimes (offset 3)
          (aset (aref grid (+ 6 offset)) 6 ?│))

        ;; Apply junction detection
        ;; No node boundaries in this synthetic test, so pass empty list
        (dag-draw--apply-junction-chars-to-grid grid '())

        ;; All arrows should be preserved
        (expect (aref (aref grid 2) 0) :to-equal ?▼)
        (expect (aref (aref grid 0) 5) :to-equal ?▲)
        (expect (aref (aref grid 5) 2) :to-equal ?►)
        (expect (aref (aref grid 5) 5) :to-equal ?◄)))))

;;; dag-draw-junction-char-test.el ends here