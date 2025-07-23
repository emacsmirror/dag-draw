;;; dag-draw-junction-char-test.el --- Tests for enhanced junction characters -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

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
          (expect (length port-junctions) :to-be-greater-than 0))))))

;;; dag-draw-junction-char-test.el ends here