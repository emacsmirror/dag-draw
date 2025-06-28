;;; dag-draw-ascii-arrows-test.el --- Tests for ASCII arrow rendering -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test-driven development for ASCII arrow functionality.
;; Following true TDD: write one failing test, make it pass, refactor, repeat.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

;; Test helper functions
(defun create-test-grid (height width)
  "Create a test ASCII grid filled with spaces."
  (let ((grid (make-vector height nil)))
    (dotimes (i height)
      (aset grid i (make-vector width ?\s)))
    grid))

(defun grid-char-at (grid x y)
  "Get character at position (X,Y) in GRID."
  (aref (aref grid y) x))

(describe "dag-draw ASCII arrows"
  (describe "arrow character selection"
    (it "should return > for rightward direction"
      (expect (dag-draw--get-arrow-char 'right) :to-equal ?>))
    
    (it "should return < for leftward direction"
      (expect (dag-draw--get-arrow-char 'left) :to-equal ?<))
    
    (it "should return v for downward direction"
      (expect (dag-draw--get-arrow-char 'down) :to-equal ?v))
    
    (it "should return ^ for upward direction"
      (expect (dag-draw--get-arrow-char 'up) :to-equal ?^)))
  
  (describe "drawing lines with arrows"
    (it "should draw horizontal line with arrow at end"
      (let ((grid (create-test-grid 1 5)))
        (dag-draw--draw-horizontal-with-arrow grid 0 0 4 0)
        (expect (grid-char-at grid 4 0) :to-equal ?>)
        (expect (grid-char-at grid 1 0) :to-equal ?─)))
    
    (it "should draw leftward horizontal line with left arrow"
      (let ((grid (create-test-grid 1 5)))
        (dag-draw--draw-horizontal-with-arrow grid 4 0 0 0)
        (expect (grid-char-at grid 0 0) :to-equal ?<)
        (expect (grid-char-at grid 3 0) :to-equal ?─))))
  
  (describe "direction detection from coordinates"
    (it "should detect rightward direction"
      (expect (dag-draw--detect-direction 1 1 5 1) :to-equal 'right))
    
    (it "should detect leftward direction"
      (expect (dag-draw--detect-direction 5 1 1 1) :to-equal 'left))
    
    (it "should detect downward direction"
      (expect (dag-draw--detect-direction 3 1 3 5) :to-equal 'down))
    
    (it "should detect upward direction"
      (expect (dag-draw--detect-direction 3 5 3 1) :to-equal 'up))))
  
  (describe "integration with graph edges"
    (it "should render simple graph with directional arrows"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'A "Node A")
        (dag-draw-add-node graph 'B "Node B")
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)
        (let ((result (dag-draw-render-ascii graph)))
          ;; Should contain arrow showing A -> B direction
          (expect result :to-match "[v>]"))))
  
  (describe "edge attachment to node boundaries"
    (it "should attach edge directly to node boundary"
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'A "Node A")
        (dag-draw-add-node graph 'B "Node B") 
        (dag-draw-add-edge graph 'A 'B)
        (dag-draw-layout-graph graph)
        (let ((result (dag-draw-render-ascii graph)))
          ;; Edge should touch node boundary, not float
          (expect result :to-match "└")    ; L-shape corner
          (expect result :to-match "v")    ; Downward arrow
          (expect result :not :to-match "\\s\\s[v>]")))))) ; Close describe blocks

(provide 'dag-draw-ascii-arrows-test)

;;; dag-draw-ascii-arrows-test.el ends here