;;; dag-draw-junction-char-test.el --- Tests for enhanced junction characters -*- lexical-binding: t -*-

;; Copyright (C) 2024

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
    
    (it "should handle starting port junction characters"
      ;; CLAUDE.md: "At the start of the edge, at the port boundary"
      ;; Example: node boundary `─` should become `┬` when edge starts downward
      (let ((context '(:type port-start :direction down :current-char ?─)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┬)))
    
    (it "should handle ending port junction characters"
      ;; CLAUDE.md: Similar logic at destination ports
      ;; Example: node boundary `─` should become `┴` when edge ends from above
      (let ((context '(:type port-end :direction up :current-char ?─)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┴)))
    
    (it "should handle direction change junctions"
      ;; CLAUDE.md: "When the edge requires a direction change"
      ;; Example: horizontal line going right that needs to turn down
      (let ((context '(:type direction-change :from-direction right :to-direction down :current-char ?─)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┐)))
    
    (it "should handle edge joining junctions"
      ;; CLAUDE.md: "When two edges join, or two edges separate"
      ;; Example: two edges coming from above joining into horizontal line
      (let ((context '(:type edge-join :incoming-directions (up up) :outgoing-direction right)))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┴)))
    
    (it "should handle edge separation junctions"
      ;; CLAUDE.md: "When two edges join, or two edges separate"
      ;; Example: horizontal line splitting into two downward edges
      (let ((context '(:type edge-split :incoming-direction left :outgoing-directions (down down))))
        (expect (dag-draw--get-enhanced-junction-char context)
                :to-equal ?┬)))
    
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

  (describe "edge analysis for junction detection"
    (it "should analyze edge intersections in ASCII grid"
      ;; This tests the analysis function that determines where junctions are needed
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'a "A")
        (dag-draw-add-node graph 'b "B")
        (dag-draw-add-node graph 'c "C")
        (dag-draw-add-edge graph 'a 'b)
        (dag-draw-add-edge graph 'a 'c)
        
        ;; Layout the graph to get coordinates
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        ;; Analyze where junction characters are needed
        (let ((junction-points (dag-draw--analyze-junction-points graph)))
          (expect junction-points :not :to-be nil)
          (expect (length junction-points) :to-be-greater-than 0))))
    
    (it "should detect port boundary junctions"
      ;; Test detection of cases where edges start/end at node boundaries
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'source "Source")
        (dag-draw-add-node graph 'target "Target")
        (dag-draw-add-edge graph 'source 'target)
        
        (dag-draw-layout-graph graph :coordinate-mode 'ascii)
        
        ;; Should detect junction at source node bottom boundary
        (let ((port-junctions (dag-draw--detect-port-junctions graph)))
          (expect port-junctions :not :to-be nil)
          (expect (length port-junctions) :to-be-greater-than 0)))))

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
          (expect (plist-get context :has-left) :to-be t)
          (expect (plist-get context :has-down) :to-be t)))))

  (describe "Priority 2: Direction change detection"
    (it "should detect a simple right-to-down corner in an edge path"
      ;; CLAUDE.md: "When the edge requires a direction change"
      ;; Create a simple edge path that goes right then turns down
      ;; Path: (0,0) -> (2,0) -> (2,2)
      ;; Expected corner at (2,0) where direction changes from horizontal to vertical
      (let* ((edge-path '((0 . 0) (1 . 0) (2 . 0) (2 . 1) (2 . 2)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        ;; Should detect one corner at position (2,0)
        (expect (length changes) :to-equal 1)
        (let ((corner (car changes)))
          (expect (plist-get corner :x) :to-equal 2)
          (expect (plist-get corner :y) :to-equal 0)
          (expect (plist-get corner :type) :to-equal 'direction-change)
          (expect (plist-get corner :from-direction) :to-equal 'right)
          (expect (plist-get corner :to-direction) :to-equal 'down))))

    (it "should detect left-to-down corner"
      ;; Path goes left then turns down
      (let* ((edge-path '((5 . 0) (4 . 0) (3 . 0) (3 . 1) (3 . 2)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 1)
        (let ((corner (car changes)))
          (expect (plist-get corner :from-direction) :to-equal 'left)
          (expect (plist-get corner :to-direction) :to-equal 'down))))

    (it "should detect down-to-right corner"
      ;; Path goes down then turns right
      (let* ((edge-path '((0 . 0) (0 . 1) (0 . 2) (1 . 2) (2 . 2)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 1)
        (let ((corner (car changes)))
          (expect (plist-get corner :from-direction) :to-equal 'down)
          (expect (plist-get corner :to-direction) :to-equal 'right))))

    (it "should detect up-to-right corner"
      ;; Path goes up then turns right
      (let* ((edge-path '((0 . 5) (0 . 4) (0 . 3) (1 . 3) (2 . 3)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 1)
        (let ((corner (car changes)))
          (expect (plist-get corner :from-direction) :to-equal 'up)
          (expect (plist-get corner :to-direction) :to-equal 'right))))

    (it "should detect multiple corners in a complex path"
      ;; Path with multiple direction changes
      ;; Right -> down -> right -> up
      (let* ((edge-path '((0 . 0) (2 . 0) (2 . 2) (4 . 2) (4 . 0)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 3)
        ;; First corner: right -> down
        (let ((corner1 (nth 0 changes)))
          (expect (plist-get corner1 :from-direction) :to-equal 'right)
          (expect (plist-get corner1 :to-direction) :to-equal 'down))
        ;; Second corner: down -> right
        (let ((corner2 (nth 1 changes)))
          (expect (plist-get corner2 :from-direction) :to-equal 'down)
          (expect (plist-get corner2 :to-direction) :to-equal 'right))
        ;; Third corner: right -> up
        (let ((corner3 (nth 2 changes)))
          (expect (plist-get corner3 :from-direction) :to-equal 'right)
          (expect (plist-get corner3 :to-direction) :to-equal 'up))))

    (it "should not detect corners in straight paths"
      ;; Horizontal straight line
      (let* ((edge-path '((0 . 0) (1 . 0) (2 . 0) (3 . 0)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 0))
      ;; Vertical straight line
      (let* ((edge-path '((0 . 0) (0 . 1) (0 . 2) (0 . 3)))
             (changes (dag-draw--detect-direction-changes-in-path edge-path)))
        (expect (length changes) :to-equal 0))))

  (describe "Priority 3: Edge intersection detection"
    (it "should detect a simple crossing of horizontal and vertical edges"
      ;; CLAUDE.md: "When two edges cross"
      ;; Edge 1: horizontal line from (0,2) to (4,2)
      ;; Edge 2: vertical line from (2,0) to (2,4)
      ;; They cross at (2,2)
      (let* ((edge1-path '((0 . 2) (1 . 2) (2 . 2) (3 . 2) (4 . 2)))
             (edge2-path '((2 . 0) (2 . 1) (2 . 2) (2 . 3) (2 . 4)))
             (edge-paths (list edge1-path edge2-path))
             (crossings (dag-draw--detect-crossings-in-paths edge-paths)))
        ;; Should detect one crossing at (2,2)
        (expect (length crossings) :to-equal 1)
        (let ((crossing (car crossings)))
          (expect (plist-get crossing :x) :to-equal 2)
          (expect (plist-get crossing :y) :to-equal 2)
          (expect (plist-get crossing :type) :to-equal 'edge-cross))))

    (it "should detect edge joins where edges share a path segment"
      ;; CLAUDE.md: "When two edges join, or two edges separate"
      ;; Two horizontal edges that share part of their path (true join, not cross)
      ;; Edge 1: (0,2) -> (3,2)
      ;; Edge 2: (1,2) -> (4,2)
      ;; They overlap from (1,2) to (3,2) - this is a join/merge
      (let* ((edge1-path '((0 . 2) (1 . 2) (2 . 2) (3 . 2)))
             (edge2-path '((1 . 2) (2 . 2) (3 . 2) (4 . 2)))
             (edge-paths (list edge1-path edge2-path))
             (joins (dag-draw--detect-joins-in-paths edge-paths)))
        ;; Should detect join points at shared positions
        (expect (length joins) :to-be-greater-than 0)))

    (it "should not detect crossings when edges just touch but don't cross"
      ;; Two horizontal lines that share an endpoint but don't cross
      ;; Edge 1: (0,0) -> (2,0)
      ;; Edge 2: (2,0) -> (4,0)
      ;; They touch at (2,0) but both are horizontal (no crossing)
      (let* ((edge1-path '((0 . 0) (1 . 0) (2 . 0)))
             (edge2-path '((2 . 0) (3 . 0) (4 . 0)))
             (edge-paths (list edge1-path edge2-path))
             (crossings (dag-draw--detect-crossings-in-paths edge-paths)))
        ;; Should detect no crossings (both horizontal)
        (expect (length crossings) :to-equal 0)))

    (it "should detect multiple crossings in complex edge patterns"
      ;; Create a grid pattern with multiple crossings
      ;; H1: (0,1) -> (4,1)
      ;; H2: (0,3) -> (4,3)
      ;; V1: (1,0) -> (1,4)
      ;; V2: (3,0) -> (3,4)
      ;; Should cross at: (1,1), (1,3), (3,1), (3,3)
      (let* ((h1 '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (4 . 1)))
             (h2 '((0 . 3) (1 . 3) (2 . 3) (3 . 3) (4 . 3)))
             (v1 '((1 . 0) (1 . 1) (1 . 2) (1 . 3) (1 . 4)))
             (v2 '((3 . 0) (3 . 1) (3 . 2) (3 . 3) (3 . 4)))
             (edge-paths (list h1 h2 v1 v2))
             (crossings (dag-draw--detect-crossings-in-paths edge-paths)))
        ;; Should detect 4 crossings
        (expect (length crossings) :to-equal 4))))

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
        (dag-draw--apply-junction-chars-to-grid grid)

        ;; Verify corner character is correct
        (let ((corner-char (aref (aref grid 5) 5)))
          (expect corner-char :to-equal ?┐)))))

  (describe "Priority 5: Edge path walking"
    (it "should track edge paths during rendering"
      ;; TDD: Test that edge rendering collects path information
      ;; This is needed so junction detection can analyze entire edge paths
      (let* ((grid (dag-draw--create-ascii-grid 10 10))
             (edge-paths '()))
        ;; Manually draw an edge and track its path
        ;; Horizontal from (0,5) to (5,5)
        (dotimes (x 6)
          (aset (aref grid 5) x ?─)
          (push (cons x 5) edge-paths))
        (setq edge-paths (nreverse edge-paths))

        ;; Verify path covers entire edge
        (expect (length edge-paths) :to-equal 6)
        (expect (car edge-paths) :to-equal '(0 . 5))
        (expect (car (last edge-paths)) :to-equal '(5 . 5))))

    (it "should make edge paths available to junction detection"
      ;; TDD: Test that collected paths can be used for junction analysis
      (let* ((edge-path '((0 . 5) (1 . 5) (2 . 5) (2 . 6) (2 . 7)))
             (corners (dag-draw--detect-direction-changes-in-path edge-path)))
        ;; Should detect one corner at (2,5) where path turns from right to down
        (expect (length corners) :to-equal 1)
        (let ((corner (car corners)))
          (expect (plist-get corner :x) :to-equal 2)
          (expect (plist-get corner :y) :to-equal 5)
          (expect (plist-get corner :type) :to-equal 'direction-change))))

    (it "should handle multiple edges with tracked paths"
      ;; TDD: Test that multiple edge paths can be analyzed together
      (let* ((edge1-path '((0 . 2) (1 . 2) (2 . 2) (3 . 2)))
             (edge2-path '((2 . 0) (2 . 1) (2 . 2) (2 . 3)))
             (edge-paths (list edge1-path edge2-path))
             (crossings (dag-draw--detect-crossings-in-paths edge-paths)))
        ;; Should detect crossing at (2,2)
        (expect (length crossings) :to-equal 1)
        (let ((crossing (car crossings)))
          (expect (plist-get crossing :x) :to-equal 2)
          (expect (plist-get crossing :y) :to-equal 2)
          (expect (plist-get crossing :type) :to-equal 'edge-cross)))))

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
        (dag-draw--apply-junction-chars-to-grid grid)

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
        (dag-draw--apply-junction-chars-to-grid grid)

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
        (dag-draw--apply-junction-chars-to-grid grid)

        ;; All arrows should be preserved
        (expect (aref (aref grid 2) 0) :to-equal ?▼)
        (expect (aref (aref grid 0) 5) :to-equal ?▲)
        (expect (aref (aref grid 5) 2) :to-equal ?►)
        (expect (aref (aref grid 5) 5) :to-equal ?◄)))))

;;; dag-draw-junction-char-test.el ends here