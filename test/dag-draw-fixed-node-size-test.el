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
              :to-equal '("This is extremely" "long text that...")))))

(provide 'dag-draw-fixed-node-size-test)

;;; dag-draw-fixed-node-size-test.el ends here