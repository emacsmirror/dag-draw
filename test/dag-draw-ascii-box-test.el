;;; dag-draw-ascii-box-test.el --- Unit tests for ASCII box drawing and text placement -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;;; Commentary:

;; GKNV Baseline Compliance Tests - ASCII: Node Box Rendering
;;
;; This module tests ASCII node box rendering as specified in
;; doc/implementation-decisions.md (ASCII adaptations).
;;
;; GKNV Reference: N/A (ASCII node rendering adapts GKNV node positioning)
;; Decision: D5.6 - Node boxes sized by label length + padding
;; Algorithm: Node Box Rendering with Unicode Box Characters
;;
;; Key Requirements Tested:
;; - Node bounding box drawn with Unicode box-drawing characters
;; - Top border: ┌──...──┐ (corners + horizontal lines)
;; - Bottom border: └──...──┘ (corners + horizontal lines)
;; - Side borders: │ (vertical lines)
;; - Label centered within box
;; - Box size: label length + padding (minimum 1 char each side)
;; - Minimum box width enforced (e.g., 5 characters)
;; - Box height typically 3 characters (top, label, bottom)
;;
;; Test Coverage:
;; - Box borders drawn with correct characters
;; - Label positioned correctly within box (centered)
;; - Box width = label length + padding
;; - Minimum width enforced for short labels
;; - Various label lengths
;; - Box rendering doesn't interfere with edges
;; - Multiple nodes with different sizes
;;
;; Baseline Status: ✅ Required for GKNV compliance (ASCII adaptation)
;;
;; See doc/implementation-decisions.md (D5.6) for decision rationale.
;; See doc/algorithm-specification.md ASCII Rendering for implementation details.

;; Tests for ASCII box drawing and text placement functionality.
;; These tests verify proper rendering of node boxes and label placement.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe
 "ASCII Box Drawing"

 (describe
  "dag-draw--ascii-draw-box"
  (it "should draw a simple 3x3 box"
      (let ((grid (dag-draw--create-ascii-grid 5 5)))
        (dag-draw--ascii-draw-box grid 1 1 3 3 nil)

        (let ((result (dag-draw--ascii-grid-to-string grid)))
          (expect result :to-match "┌─┐")
          (expect result :to-match "│ │")
          (expect result :to-match "└─┘"))))

  (it "should draw correct box corners and edges"
      (let ((grid (dag-draw--create-ascii-grid 4 3)))
        (dag-draw--ascii-draw-box grid 0 0 4 3 nil)

        ;; Check specific characters
        (expect (aref (aref grid 0) 0) :to-equal ?┌)  ; top-left
        (expect (aref (aref grid 0) 1) :to-equal ?─)  ; top edge
        (expect (aref (aref grid 0) 3) :to-equal ?┐)  ; top-right
        (expect (aref (aref grid 1) 0) :to-equal ?│)  ; left edge
        (expect (aref (aref grid 1) 3) :to-equal ?│)  ; right edge
        (expect (aref (aref grid 2) 0) :to-equal ?└)  ; bottom-left
        (expect (aref (aref grid 2) 1) :to-equal ?─)  ; bottom edge
        (expect (aref (aref grid 2) 3) :to-equal ?┘))) ; bottom-right

  (it "should handle minimum box size of 3x3"
      (let ((grid (dag-draw--create-ascii-grid 5 5)))
        (dag-draw--ascii-draw-box grid 1 1 3 3 nil)

        ;; Verify the box structure
        (expect (aref (aref grid 1) 1) :to-equal ?┌)
        (expect (aref (aref grid 1) 2) :to-equal ?─)
        (expect (aref (aref grid 1) 3) :to-equal ?┐)
        (expect (aref (aref grid 2) 1) :to-equal ?│)
        (expect (aref (aref grid 2) 3) :to-equal ?│)
        (expect (aref (aref grid 3) 1) :to-equal ?└)
        (expect (aref (aref grid 3) 2) :to-equal ?─)
        (expect (aref (aref grid 3) 3) :to-equal ?┘)))

  (it "should draw larger boxes correctly"
      (let ((grid (dag-draw--create-ascii-grid 8 6)))
        (dag-draw--ascii-draw-box grid 1 1 6 4 nil)

        ;; Check that top and bottom edges span correctly
        (expect (aref (aref grid 1) 1) :to-equal ?┌)
        (expect (aref (aref grid 1) 2) :to-equal ?─)
        (expect (aref (aref grid 1) 3) :to-equal ?─)
        (expect (aref (aref grid 1) 4) :to-equal ?─)
        (expect (aref (aref grid 1) 5) :to-equal ?─)
        (expect (aref (aref grid 1) 6) :to-equal ?┐)

        ;; Check side edges
        (expect (aref (aref grid 2) 1) :to-equal ?│)
        (expect (aref (aref grid 2) 6) :to-equal ?│)
        (expect (aref (aref grid 3) 1) :to-equal ?│)
        (expect (aref (aref grid 3) 6) :to-equal ?│)))

  (it "should handle box at grid boundaries"
      (let ((grid (dag-draw--create-ascii-grid 4 3)))
        (dag-draw--ascii-draw-box grid 0 0 4 3 nil)

        ;; Should fit exactly within grid
        (expect (aref (aref grid 0) 0) :to-equal ?┌)
        (expect (aref (aref grid 0) 3) :to-equal ?┐)
        (expect (aref (aref grid 2) 0) :to-equal ?└)
        (expect (aref (aref grid 2) 3) :to-equal ?┘)))

  (it "should handle boxes extending beyond grid boundaries"
      (let ((grid (dag-draw--create-ascii-grid 3 3)))
        ;; Try to draw a 5x5 box starting at (0,0) - should be clipped
        (dag-draw--ascii-draw-box grid 0 0 5 5 nil)

        ;; Should only draw what fits
        (expect (aref (aref grid 0) 0) :to-equal ?┌)
        (expect (aref (aref grid 0) 1) :to-equal ?─)
        (expect (aref (aref grid 0) 2) :to-equal ?─)
        (expect (aref (aref grid 1) 0) :to-equal ?│)
        (expect (aref (aref grid 2) 0) :to-equal ?│)))

  (it "should handle negative positions gracefully"
      (let ((grid (dag-draw--create-ascii-grid 5 5)))
        ;; Try to draw box starting at negative position
        (dag-draw--ascii-draw-box grid -1 -1 4 4 nil)

        ;; Should only draw the visible part
        (expect (aref (aref grid 0) 0) :to-equal ?┘)  ; what would be bottom-right of box
        (expect (aref (aref grid 0) 1) :to-equal ?\s)
        (expect (aref (aref grid 1) 0) :to-equal ?\s))))

 (describe
  "ASCII Text Placement"

  (describe
   "text placement within boxes"
   (it "should center short text in box"
       (let ((grid (dag-draw--create-ascii-grid 7 3)))
         (dag-draw--ascii-draw-box grid 1 0 5 3 "Hi")

         (let ((result (dag-draw--ascii-grid-to-string grid)))
           ;; Text should be centered in the middle row
           (expect result :to-match "Hi"))))

   (it "should center single character"
       (let ((grid (dag-draw--create-ascii-grid 5 3)))
         (dag-draw--ascii-draw-box grid 0 0 5 3 "X")

         ;; Find where X is placed
         (let ((found-x nil))
           (dotimes (y 3)
             (dotimes (x 5)
               (when (eq (aref (aref grid y) x) ?X)
                 (setq found-x (list x y)))))

           ;; Should be centered: middle of 5-wide box is position 2, middle of 3-high is row 1
           (expect found-x :to-equal '(2 1)))))

   (it "should handle text that fits exactly"
       (let ((grid (dag-draw--create-ascii-grid 7 3)))
         (dag-draw--ascii-draw-box grid 1 0 5 3 "ABC")

         ;; Text "ABC" (3 chars) in 5-wide box should be centered at position 2
         (expect (aref (aref grid 1) 2) :to-equal ?A)
         (expect (aref (aref grid 1) 3) :to-equal ?B)
         (expect (aref (aref grid 1) 4) :to-equal ?C)))

   (it "should not draw text in boxes too small"
       (let ((grid (dag-draw--create-ascii-grid 3 3)))
         ;; 3x3 box has interior size 1x1, too small for any text
         (dag-draw--ascii-draw-box grid 0 0 3 3 "X")

         ;; Only box characters should be present, no text
         (expect (aref (aref grid 1) 1) :to-equal ?\s)))

   (it "should truncate text that is too long"
       (let ((grid (dag-draw--create-ascii-grid 7 3)))
         ;; 7-wide box has interior width of 5, try to place 6-char text
         (dag-draw--ascii-draw-box grid 0 0 7 3 "TOOLONG")

         ;; Should only place characters that fit within box interior
         (let ((text-in-box ""))
           (dotimes (x 7)
             (let ((char (aref (aref grid 1) x)))
               (when (and (not (memq char '(?┌ ?─ ?┐ ?│ ?└ ?┘ ?\s)))
                          (>= char ?A) (<= char ?Z))
                 (setq text-in-box (concat text-in-box (string char))))))

           ;; Should contain truncated text
           (expect (length text-in-box) :to-be-less-than 7))))

   (it "should not overwrite box borders"
       (let ((grid (dag-draw--create-ascii-grid 5 3)))
         (dag-draw--ascii-draw-box grid 0 0 5 3 "HELLO")

         ;; Verify box borders are still intact
         (expect (aref (aref grid 0) 0) :to-equal ?┌)
         (expect (aref (aref grid 0) 4) :to-equal ?┐)
         (expect (aref (aref grid 2) 0) :to-equal ?└)
         (expect (aref (aref grid 2) 4) :to-equal ?┘)

         ;; Verify side borders
         (expect (aref (aref grid 1) 0) :to-equal ?│)
         (expect (aref (aref grid 1) 4) :to-equal ?│))))

  (describe
   "ASCII Line Drawing"

   (describe
    "dag-draw--ascii-draw-line"
    (it "should draw horizontal line"
        (let ((grid (dag-draw--create-ascii-grid 5 3)))
          (dag-draw--ascii-draw-line grid 1 1 3 1)

          (expect (aref (aref grid 1) 1) :to-equal ?─)
          (expect (aref (aref grid 1) 2) :to-equal ?─)
          (expect (aref (aref grid 1) 3) :to-equal ?─)))

    (it "should draw vertical line"
        (let ((grid (dag-draw--create-ascii-grid 3 5)))
          (dag-draw--ascii-draw-line grid 1 1 1 3)

          (expect (aref (aref grid 1) 1) :to-equal ?│)
          (expect (aref (aref grid 2) 1) :to-equal ?│)
          (expect (aref (aref grid 3) 1) :to-equal ?│)))

    (it "should draw L-shaped line (horizontal then vertical)"
        (let ((grid (dag-draw--create-ascii-grid 5 5)))
          (dag-draw--ascii-draw-line grid 1 1 3 3)

          ;; Should draw horizontal first, then vertical
          (expect (aref (aref grid 1) 1) :to-equal ?─)
          (expect (aref (aref grid 1) 2) :to-equal ?─)
          (expect (aref (aref grid 1) 3) :to-equal ?─)
          (expect (aref (aref grid 2) 3) :to-equal ?│)
          (expect (aref (aref grid 3) 3) :to-equal ?│)))

    (it "should handle single point line"
        (let ((grid (dag-draw--create-ascii-grid 3 3)))
          (dag-draw--ascii-draw-line grid 1 1 1 1)

          ;; Single point should still draw something
          (expect (aref (aref grid 1) 1) :not :to-equal ?\s)))

    (it "should handle lines going backwards"
        (let ((grid (dag-draw--create-ascii-grid 5 3)))
          (dag-draw--ascii-draw-line grid 3 1 1 1)

          ;; Should draw horizontal line regardless of direction
          (expect (aref (aref grid 1) 1) :to-equal ?─)
          (expect (aref (aref grid 1) 2) :to-equal ?─)
          (expect (aref (aref grid 1) 3) :to-equal ?─)))

    (it "should handle lines outside grid boundaries"
        (let ((grid (dag-draw--create-ascii-grid 3 3)))
          ;; Try to draw line that extends beyond grid
          (dag-draw--ascii-draw-line grid 1 1 5 1)

          ;; Should only draw what fits
          (expect (aref (aref grid 1) 1) :to-equal ?─)
          (expect (aref (aref grid 1) 2) :to-equal ?─)
          ;; Position 3 and beyond are outside grid bounds
          )))

   (describe
    "Complex Box and Text Scenarios"

    (it "should handle multiple boxes without interference"
        (let ((grid (dag-draw--create-ascii-grid 10 5)))
          (dag-draw--ascii-draw-box grid 0 0 4 3 "A")
          (dag-draw--ascii-draw-box grid 5 2 4 3 "B")

          ;; Both boxes should be present
          (expect (aref (aref grid 0) 0) :to-equal ?┌)  ; Box A
          (expect (aref (aref grid 2) 5) :to-equal ?┌)  ; Box B

          ;; Both labels should be present
          (let ((result (dag-draw--ascii-grid-to-string grid)))
            (expect result :to-match "A")
            (expect result :to-match "B"))))

    (it "should handle overlapping boxes gracefully"
        (let ((grid (dag-draw--create-ascii-grid 6 4)))
          (dag-draw--ascii-draw-box grid 0 0 4 3 "X")
          (dag-draw--ascii-draw-box grid 2 1 4 3 "Y")

          ;; Second box should overwrite overlapping area
          (expect (aref (aref grid 1) 2) :to-equal ?┌)  ; Second box's corner
          ))

    (it "should preserve spacing between non-overlapping elements"
        (let ((grid (dag-draw--create-ascii-grid 8 3)))
          (dag-draw--ascii-draw-box grid 0 0 3 3 "L")
          (dag-draw--ascii-draw-box grid 5 0 3 3 "R")

          ;; Space between boxes should remain
          (expect (aref (aref grid 1) 3) :to-equal ?\s)
          (expect (aref (aref grid 1) 4) :to-equal ?\s)))

    (it "should handle empty labels"
        (let ((grid (dag-draw--create-ascii-grid 5 3)))
          (dag-draw--ascii-draw-box grid 0 0 5 3 "")

          ;; Box should be drawn, interior should be empty
          (expect (aref (aref grid 0) 0) :to-equal ?┌)
          (expect (aref (aref grid 1) 2) :to-equal ?\s)))  ; Center should be space

    (it "should handle nil labels"
        (let ((grid (dag-draw--create-ascii-grid 5 3)))
          (dag-draw--ascii-draw-box grid 0 0 5 3 nil)

          ;; Box should be drawn, interior should be empty
          (expect (aref (aref grid 0) 0) :to-equal ?┌)
          (expect (aref (aref grid 1) 2) :to-equal ?\s))))))) ; Center should be space

;;; dag-draw-ascii-box-test.el ends here
