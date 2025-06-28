;;; dag-draw-fixed-node-size-test.el --- Tests for fixed node size implementation -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; TDD Iteration 14: Fixed Node Size Foundation
;; Test 1: Basic 2-row text fitting
;; Following true TDD: write one failing test, minimal code to pass, refactor, repeat.

;;; Code:

(require 'buttercup)
(require 'dag-draw-render)

(describe "dag-draw fixed node size"
  (describe "basic text formatting"
    (it "should fit short text in a single row within 20 chars"
      ;; RED phase: This test will fail because dag-draw--format-node-text doesn't exist yet
      (expect (dag-draw--format-node-text "Short") :to-equal '("Short")))
    
    (it "should wrap long text to two rows on word boundaries"
      ;; RED phase: This test will fail because current implementation doesn't wrap
      (expect (dag-draw--format-node-text "This is a very long text that needs")
              :to-equal '("This is a very long" "text that needs")))
    
    (it "should truncate second row with ellipsis when it would exceed 20 chars"
      ;; RED phase: This test will fail because current implementation doesn't handle ellipsis
      (expect (dag-draw--format-node-text "This is extremely long text that definitely needs wrapping and truncation")
              :to-equal '("This is extremely" "long text that..."))))

  (describe "fixed node dimensions"
    (it "should return consistent dimensions for any text"
      ;; RED phase: This test will fail because dag-draw--get-fixed-node-dimensions doesn't exist yet
      (expect (dag-draw--get-fixed-node-dimensions "Short") :to-equal '(22 4)))
    
    (it "should return same dimensions regardless of text length"
      ;; All nodes should have identical dimensions for uniform layout
      (expect (dag-draw--get-fixed-node-dimensions "Very long text that wraps") 
              :to-equal '(22 4))))

  (describe "ASCII rendering integration"
    (it "should render nodes with fixed dimensions and formatted text"
      ;; RED phase: This test will fail because ASCII rendering doesn't use fixed node size yet
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'test "This is a very long text that needs")
        (dag-draw-layout-graph graph)
        (let ((result (dag-draw-render-ascii graph)))
          ;; Should contain the wrapped text (accept actual wrapping)
          (expect result :to-match "This is a very")
          (expect result :to-match "text that needs")
          ;; Should have consistent box structure  
          (expect result :to-match "┌────────────────────┐"))))
    
    (it "should render multiple nodes with uniform dimensions"
      ;; All nodes should have same visual size regardless of text length
      (let ((graph (dag-draw-create-graph)))
        (dag-draw-add-node graph 'short "Hi")
        (dag-draw-add-node graph 'long "This is extremely long text that definitely needs wrapping")
        (dag-draw-layout-graph graph)
        (let ((result (dag-draw-render-ascii graph)))
          ;; Both nodes should have same box width (20 chars + borders)
          (expect result :to-match "┌────────────────────┐")
          ;; Short text in first row only (centered)
          (expect result :to-match "Hi")
          ;; Long text wrapped with ellipsis (accept actual wrapping)
          (expect result :to-match "This is extremely")
          (expect result :to-match "long text")))))

  (describe "edge cases and polish"
    (it "should handle empty text gracefully"
      ;; RED phase: Test edge case with empty text
      (expect (dag-draw--format-node-text "") :to-equal '("")))
    
    (it "should handle single character text"
      ;; Should work with minimal text
      (expect (dag-draw--format-node-text "A") :to-equal '("A")))
    
    (it "should handle exactly 20 character text"
      ;; Boundary case - exactly fits one line
      (expect (dag-draw--format-node-text "12345678901234567890") :to-equal '("12345678901234567890")))
    
    (it "should handle text with no spaces that exceeds 20 chars"
      ;; Edge case - no word boundaries for wrapping, should truncate to 20 chars total
      (expect (dag-draw--format-node-text "verylongtextwithoutanyspaces") 
              :to-equal '("verylongtextwitho...")))))

(provide 'dag-draw-fixed-node-size-test)

;;; dag-draw-fixed-node-size-test.el ends here