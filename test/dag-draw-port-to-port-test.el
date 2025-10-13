;;; dag-draw-port-to-port-test.el --- Tests for precise port-to-port edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; FUTURE ENHANCEMENT - Beyond GKNV Baseline
;;
;; Enhancement Category: Advanced Feature
;; Baseline Status: ⏳ Deferred (Enhanced port-based edge routing)
;;
;; This test verifies:
;; - Precise port-to-port edge routing
;; - Clean visual connections at node boundaries
;; - Port-based connection quality in ASCII output
;;
;; Related Baseline Decisions: D4.x (Splines), D5.x (ASCII Rendering)
;; Enhancement Source: Enhanced edge routing precision
;;
;; GKNV baseline uses general node boundaries.
;; This tests precise port-based routing for cleaner output.
;; These enhancements may be implemented in Phase 5 (Future Work).
;; See doc/test-suite-analysis.md for categorization rationale.
;;
;; [Original commentary: Tests to verify that edges connect at precise node boundary ports...]
;;
;; Tests to verify that edges connect at precise node boundary ports
;; for cleaner visual connections in ASCII output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)
(require 'dag-draw-test-harness)

(describe "Port-to-Port Edge Routing"
  
  (it "should produce clean ASCII connections with port routing"
    (let ((graph (dag-draw-create-graph)))
      ;; Create a simple chain to test connection quality
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'middle)
      (dag-draw-add-edge graph 'middle 'end)
      
      ;; Run full layout
      (dag-draw-layout-graph graph)
      
      ;; Generate ASCII output
      (let ((output (dag-draw-render-ascii graph)))
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((connectivity-validation (dag-draw-test--validate-edge-connectivity output graph)))
          (expect (plist-get connectivity-validation :all-connected) :to-be t))
        (let ((arrow-validation (dag-draw-test--validate-arrows output)))
          (expect (plist-get arrow-validation :valid-arrows) :to-be-greater-than 0))
        
        ;; Debug output to verify visual quality
        (message "\n=== PORT-TO-PORT ROUTING OUTPUT ===")
        (message "%s" output)
        (message "===================================\n"))))
  
  (it "should handle diamond routing pattern"
    (let ((graph (dag-draw-create-graph)))
      ;; Create diamond pattern
      (dag-draw-add-node graph 'top "Top")
      (dag-draw-add-node graph 'left "Left")
      (dag-draw-add-node graph 'right "Right")
      (dag-draw-add-node graph 'bottom "Bottom")
      
      (dag-draw-add-edge graph 'top 'left)
      (dag-draw-add-edge graph 'top 'right)
      (dag-draw-add-edge graph 'left 'bottom)
      (dag-draw-add-edge graph 'right 'bottom)
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Generate ASCII output
      (let ((output (dag-draw-render-ascii graph)))
        
        ;; Use test harness for comprehensive validation
        (let ((node-validation (dag-draw-test--validate-node-completeness output graph)))
          (expect (plist-get node-validation :complete) :to-be t))
        (let ((boundary-validation (dag-draw-test--validate-node-boundaries output)))
          (expect (plist-get boundary-validation :valid) :to-be t))
        (let ((structure-validation (dag-draw-test--validate-graph-structure output graph)))
          (expect (plist-get structure-validation :topology-match) :to-be t))
        
        ;; Debug output
        (message "\n=== DIAMOND ROUTING OUTPUT ===")
        (message "%s" output)
        (message "==============================\n")))))

;;; dag-draw-port-to-port-test.el ends here