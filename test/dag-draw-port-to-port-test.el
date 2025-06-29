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
        
        ;; Should show all nodes
        (expect output :to-match "Start")
        (expect output :to-match "Middle")
        (expect output :to-match "End")
        
        ;; Should have clean vertical connections
        (expect (string-match-p "│" output) :to-be-truthy)
        (expect (string-match-p "v" output) :to-be-truthy)
        
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
        
        ;; Should show all nodes
        (expect output :to-match "Top")
        (expect output :to-match "Left")
        (expect output :to-match "Right")
        (expect output :to-match "Bottom")
        
        ;; Should have various connection types
        (expect (string-match-p "[│─┌┐└┘]" output) :to-be-truthy)
        
        ;; Debug output
        (message "\n=== DIAMOND ROUTING OUTPUT ===")
        (message "%s" output)
        (message "==============================\n")))))

;;; dag-draw-port-to-port-test.el ends here