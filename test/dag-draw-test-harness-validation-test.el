;;; dag-draw-test-harness-validation-test.el --- Tests for test harness utilities -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Meta-tests for the test harness utilities themselves.
;; These tests verify that our grid-based test utilities work correctly.

;;; Code:

(require 'buttercup)
(require 'dag-draw-test-harness)

;;; Spatial Verification Tests (with Graph Integration)

(describe "dag-draw-test--verify-hierarchical-y-coords"
  (it "should verify Y-coordinates follow GKNV formula Y = rank * ranksep"
    (let* ((ascii-grid "┌───┐
│ A │
└───┘

┌───┐
│ B │
└───┘")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           ;; Create mock graph structure
           (mock-graph (make-hash-table :test 'equal)))
      ;; Add mock nodes with rank information
      (puthash 'nodes (list (list :id 'a :label "A" :rank 0 :y 0)
                            (list :id 'b :label "B" :rank 1 :y 4))
               mock-graph)
      (puthash 'ranksep 4 mock-graph)

      (let ((result (dag-draw-test--verify-hierarchical-y-coords mock-graph grid 1.0)))
        (expect (plist-get result :correct) :to-be t)
        (expect (length (plist-get result :violations)) :to-equal 0)))))

(describe "dag-draw-test--verify-separation-constraints"
  (it "should verify X-separation follows GKNV D3.3 formula"
    (let* ((ascii-grid "┌───┐     ┌───┐
│ A │     │ B │
└───┘     └───┘")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (mock-graph (make-hash-table :test 'equal)))
      ;; Add mock nodes on same rank
      (puthash 'nodes (list (list :id 'a :label "A" :rank 0 :x 0 :xsize 5)
                            (list :id 'b :label "B" :rank 0 :x 10 :xsize 5))
               mock-graph)
      (puthash 'nodesep 5 mock-graph)

      (let ((result (dag-draw-test--verify-separation-constraints mock-graph grid 1.0)))
        (expect (plist-get result :all-satisfied) :to-be t)
        (expect (length (plist-get result :violations)) :to-equal 0)))))

;;; Junction Validation Tests

(describe "dag-draw-test--find-malformed-junctions"
  (it "should detect junction adjacent to node border"
    (let* ((ascii-grid "┌──┼│")  ; Malformed: junction ┼ adjacent to border │
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (malformed (dag-draw-test--find-malformed-junctions grid)))
      (expect (length malformed) :to-be-greater-than 0)))

  (it "should return empty list for well-formed junctions"
    (let* ((ascii-grid "┌───┐
│   │
└─┬─┘
  │")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (malformed (dag-draw-test--find-malformed-junctions grid)))
      (expect (length malformed) :to-equal 0))))

(describe "dag-draw-test--validate-junction-connectivity"
  (it "should validate corner junctions connect exactly 2 directions"
    (let* ((ascii-grid "  │
  └──")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (result (dag-draw-test--validate-junction-connectivity grid)))
      (expect (plist-get result :all-valid) :to-be t)
      (expect (length (plist-get result :invalid-junctions)) :to-equal 0)))

  (it "should detect T-junction with wrong connection count"
    (let* ((ascii-grid "  ┬  ")  ; T-junction with no connections (invalid)
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (result (dag-draw-test--validate-junction-connectivity grid)))
      (expect (plist-get result :all-valid) :to-be nil)
      (expect (length (plist-get result :invalid-junctions)) :to-be-greater-than 0))))

(describe "dag-draw-test--validate-junction-border-separation"
  (it "should detect junctions adjacent to node borders"
    (let* ((ascii-grid "┌──┼│")  ; Junction adjacent to border
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (result (dag-draw-test--validate-junction-border-separation grid)))
      (expect (plist-get result :no-adjacency) :to-be nil)
      (expect (length (plist-get result :adjacent-junctions)) :to-be-greater-than 0)))

  (it "should pass when junctions are clearly separated"
    (let* ((ascii-grid "  │
  │")  ; Vertical edge with no borders nearby
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (result (dag-draw-test--validate-junction-border-separation grid)))
      ;; No junction characters in this grid, so no adjacency issues
      (expect (plist-get result :no-adjacency) :to-be t)
      (expect (length (plist-get result :adjacent-junctions)) :to-equal 0))))

(describe "dag-draw-test--trace-edge-junctions"
  (it "should trace junctions along an edge path"
    (let* ((ascii-grid "┌───┐
│ A │
└─┬─┘
  │
┌─▼─┐
│ B │
└───┘")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (start-pos (cons 2 2))  ; Position of ┬ junction
           (end-pos (cons 2 4))    ; Position of ▼ arrow
           (junctions (dag-draw-test--trace-edge-junctions grid start-pos end-pos)))
      (expect (length junctions) :to-be-greater-than 0))))

(describe "dag-draw-test--measure-grid-distance"
  (it "should calculate Manhattan distance between two grid positions"
    (let ((pos1 (cons 0 0))
          (pos2 (cons 3 4)))
      (let ((distance (dag-draw-test--measure-grid-distance nil pos1 pos2)))
        (expect (car distance) :to-equal 3)
        (expect (cdr distance) :to-equal 4)))))

;;; Edge Path Analysis Tests

(describe "dag-draw-test--verify-path-continuity"
  (it "should return :continuous t for a continuous path"
    (let ((path '((0 0) (0 1) (0 2) (1 2))))
      (let ((result (dag-draw-test--verify-path-continuity nil path)))
        (expect (plist-get result :continuous) :to-be t)
        (expect (length (plist-get result :gaps)) :to-equal 0))))

  (it "should detect gaps in a discontinuous path"
    (let ((path '((0 0) (0 1) (0 3))))  ; Gap between (0 1) and (0 3)
      (let ((result (dag-draw-test--verify-path-continuity nil path)))
        (expect (plist-get result :continuous) :to-be nil)
        (expect (length (plist-get result :gaps)) :to-be-greater-than 0)))))

(describe "dag-draw-test--verify-orthogonal-routing"
  (it "should return :orthogonal t for paths with only horizontal and vertical segments"
    (let ((path '((0 0) (0 1) (0 2) (1 2) (2 2))))  ; Vertical then horizontal
      (let ((result (dag-draw-test--verify-orthogonal-routing nil path)))
        (expect (plist-get result :orthogonal) :to-be t)
        (expect (length (plist-get result :diagonal-segments)) :to-equal 0))))

  (it "should detect diagonal segments"
    (let ((path '((0 0) (1 1))))  ; Diagonal move
      (let ((result (dag-draw-test--verify-orthogonal-routing nil path)))
        (expect (plist-get result :orthogonal) :to-be nil)
        (expect (length (plist-get result :diagonal-segments)) :to-be-greater-than 0)))))

(describe "dag-draw-test--get-edge-path"
  (it "should find edge path between two nodes in a grid"
    (let* ((ascii-grid "┌───┐
│ A │
└─┬─┘
  │
┌─▼─┐
│ B │
└───┘")
           (grid (dag-draw-test--parse-ascii-grid ascii-grid))
           (from-node (list :x 0 :y 0 :width 5 :height 3 :text "A"))
           (to-node (list :x 0 :y 4 :width 5 :height 3 :text "B"))
           (path (dag-draw-test--get-edge-path grid from-node to-node)))
      (expect path :not :to-be nil)
      (expect (length path) :to-be-greater-than 0))))

(provide 'dag-draw-test-harness-validation-test)

;;; dag-draw-test-harness-validation-test.el ends here
