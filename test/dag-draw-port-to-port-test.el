;;; dag-draw-port-to-port-test.el --- Tests for precise port-to-port edge routing -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

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