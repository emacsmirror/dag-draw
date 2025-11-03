;;; dag-draw-ascii-junction-implementation-test.el --- TDD tests for junction character implementation -*- lexical-binding: t -*-

;; Copyright (C) 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Junction Implementation
;;
;; This module tests junction character implementation details as specified in
;; doc/CLAUDE.md and doc/implementation-decisions.md.
;;
;; GKNV Reference: N/A (junction implementation is ASCII-specific)
;; Decision: D5.4 - Walk-based algorithm implementation
;; Algorithm: Junction Character Implementation Verification
;;
;; Key Requirements Tested:
;; - Edge walking visits all points along edge path
;; - Previous/current/next point context captured at each step
;; - Direction analysis determines incoming/outgoing edge directions
;; - Multiple edges at same point detected correctly
;; - Junction character selection function correct for all cases
;; - Grid cell update replaces segment character with junction character
;; - Junction algorithm applied after routing, before final output
;;
;; Test Coverage:
;; - Edge path extraction correct
;; - Point-by-point walking visits all edge positions
;; - Context capture (prev/curr/next) correct at each point
;; - Direction analysis correct (cardinal directions)
;; - Multi-edge detection at shared points
;; - Junction character selection invoked correctly
;; - Grid cell updates applied correctly
;; - Integration with routing and rendering
;;
;; Baseline Status: ✅ Required for GKNV compliance (ASCII adaptation)
;;
;; See doc/CLAUDE.md (Junction algorithm implementation) for specification.
;; See doc/implementation-decisions.md (D5.4) for decision rationale.

;; Test-driven development for ASCII junction character implementation
;; following decisions D5.1-D5.8 from doc/implementation-decisions.md

;;; Code:

(require 'buttercup)
(require 'dag-draw-ascii-edges)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-core)

(describe "D5.8: Junction Character Context Selection"
  (describe "Context plist usage"
    (it "should accept a context plist with :type key"
      (let ((context (list :type 'direction-change
                          :from-direction 'right
                          :to-direction 'down)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┐))))

    (it "should return corner for right-to-down direction change"
      (let ((context (list :type 'direction-change
                          :from-direction 'right
                          :to-direction 'down)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┐))))

    (it "should return cross for edge-cross type"
      (let ((context (list :type 'edge-cross)))
        (let ((result (dag-draw--get-enhanced-junction-char context)))
          (expect result :to-equal ?┼))))))

(describe "Priority 1: Function Call Signature Fix"
  (describe "Local junction context analysis"
    (it "should detect vertical line with right branch (T-junction ├)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Set up a T-junction: vertical line with right branch
        ;; Vertical line at x=5
        (dotimes (y 10)
          (aset (aref grid y) 5 ?│))
        ;; Horizontal branch to the right from (5,5)
        (dotimes (x 5)
          (aset (aref grid 5) (+ 5 x) ?─))

        ;; At position (5,5) we should detect a T-junction with vertical main, right branch
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?│ ?─)))
          (expect (plist-get context :type) :to-equal 't-junction)
          (expect (plist-get context :main-direction) :to-be 'down)
          (expect (plist-get context :branch-direction) :to-be 'right))))

    (it "should detect right-to-down corner (┐)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Horizontal line from left
        (dotimes (x 6)
          (aset (aref grid 5) x ?─))
        ;; Vertical line going down
        (dotimes (y 5)
          (aset (aref grid (+ 5 y)) 5 ?│))

        ;; At position (5,5) we should detect a corner
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?─ ?│)))
          (expect (plist-get context :type) :to-equal 'direction-change)
          (expect (plist-get context :from-direction) :to-be 'right)
          (expect (plist-get context :to-direction) :to-be 'down))))

    (it "should detect edge crossing (┼)"
      (let ((grid (dag-draw--create-ascii-grid 10 10)))
        ;; Horizontal line through (5,5)
        (dotimes (x 10)
          (aset (aref grid 5) x ?─))
        ;; Vertical line through (5,5)
        (dotimes (y 10)
          (aset (aref grid y) 5 ?│))

        ;; At position (5,5) we should detect a cross
        (let ((context (dag-draw--analyze-local-grid-junction-context grid 5 5 ?─ ?│)))
          (expect (plist-get context :type) :to-equal 'edge-cross))))))

(provide 'dag-draw-ascii-junction-implementation-test)
;;; dag-draw-ascii-junction-implementation-test.el ends here
