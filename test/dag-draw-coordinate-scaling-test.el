;;; dag-draw-coordinate-scaling-test.el --- Tests for GKNV to ASCII coordinate scaling -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Tests to verify the integration between GKNV algorithm coordinates 
;; and ASCII grid rendering produces optimal visual output.

;;; Code:

(require 'buttercup)
(require 'dag-draw)
(require 'dag-draw-core)
(require 'dag-draw-render)

(describe "GKNV to ASCII Coordinate Scaling Integration"
  
  (it "should produce appropriately sized nodes for text content"
    (let ((graph (dag-draw-create-graph)))
      ;; Test nodes with different text lengths
      (dag-draw-add-node graph 'short "A")
      (dag-draw-add-node graph 'medium "Medium Text")
      (dag-draw-add-node graph 'long "Very Long Node Label")
      
      ;; Run full GKNV layout
      (dag-draw-layout-graph graph)
      
      ;; Check that node sizes are appropriate for their content
      (let ((short-node (dag-draw-get-node graph 'short))
            (medium-node (dag-draw-get-node graph 'medium))
            (long-node (dag-draw-get-node graph 'long)))
        
        ;; Node sizes should increase with text length
        (expect (dag-draw-node-x-size short-node) :to-be-less-than 
                (dag-draw-node-x-size medium-node))
        (expect (dag-draw-node-x-size medium-node) :to-be-less-than 
                (dag-draw-node-x-size long-node))
        
        ;; ASCII rendering should show complete text
        (let ((output (dag-draw-render-ascii graph)))
          (expect output :to-match "A")
          (expect output :to-match "Medium Text")
          (expect output :to-match "Very Long Node Label")))))
  
  (it "should maintain proper spacing between nodes"
    (let ((graph (dag-draw-create-graph)))
      (dag-draw-add-node graph 'top "Top Node")
      (dag-draw-add-node graph 'bottom "Bottom Node")
      (dag-draw-add-edge graph 'top 'bottom)
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Get node coordinates
      (let* ((top-node (dag-draw-get-node graph 'top))
             (bottom-node (dag-draw-get-node graph 'bottom))
             (y-separation (- (dag-draw-node-y-coord bottom-node)
                             (dag-draw-node-y-coord top-node))))
        
        ;; Should have reasonable separation (updated for improved compact layout)
        (expect y-separation :to-be-greater-than 24)  ; Default minimum separation for compact layout
        (expect y-separation :to-be-less-than 150)    ; Maximum reasonable separation
        
        ;; ASCII output should show clear separation
        (let ((output (dag-draw-render-ascii graph)))
          (expect output :to-match "Top Node")
          (expect output :to-match "Bottom Node")
          ;; Should have connecting lines
          (expect (string-match-p "[│v]" output) :to-be-truthy)))))
  
  (it "should handle coordinate scaling with edge routing"
    (let ((graph (dag-draw-create-graph)))
      ;; Create L-shaped routing scenario
      (dag-draw-add-node graph 'start "Start")
      (dag-draw-add-node graph 'middle "Middle")
      (dag-draw-add-node graph 'end "End")
      (dag-draw-add-edge graph 'start 'middle)
      (dag-draw-add-edge graph 'middle 'end)
      
      ;; Run layout
      (dag-draw-layout-graph graph)
      
      ;; Generate ASCII output
      (let ((output (dag-draw-render-ascii graph)))
        
        ;; Should show all nodes clearly
        (expect output :to-match "Start")
        (expect output :to-match "Middle") 
        (expect output :to-match "End")
        
        ;; Should have edge connections
        (expect (string-match-p "[│─┌┐└┘v<>^]" output) :to-be-truthy)
        
        ;; Debug output
        (message "\n=== COORDINATE SCALING TEST OUTPUT ===")
        (message "%s" output)
        (message "=====================================\n"))))
  
  (it "should optimize grid density for different graph complexities"
    (let ((simple-graph (dag-draw-create-graph))
          (complex-graph (dag-draw-create-graph)))
      
      ;; Simple graph: 2 nodes
      (dag-draw-add-node simple-graph 'a "Node A")
      (dag-draw-add-node simple-graph 'b "Node B")
      (dag-draw-add-edge simple-graph 'a 'b)
      
      ;; Complex graph: 6 nodes in diamond pattern
      (dag-draw-add-node complex-graph 'top "Top")
      (dag-draw-add-node complex-graph 'left "Left")
      (dag-draw-add-node complex-graph 'right "Right")
      (dag-draw-add-node complex-graph 'mid-left "Mid Left")
      (dag-draw-add-node complex-graph 'mid-right "Mid Right")
      (dag-draw-add-node complex-graph 'bottom "Bottom")
      
      (dag-draw-add-edge complex-graph 'top 'left)
      (dag-draw-add-edge complex-graph 'top 'right)
      (dag-draw-add-edge complex-graph 'left 'mid-left)
      (dag-draw-add-edge complex-graph 'right 'mid-right)
      (dag-draw-add-edge complex-graph 'mid-left 'bottom)
      (dag-draw-add-edge complex-graph 'mid-right 'bottom)
      
      ;; Run layouts
      (dag-draw-layout-graph simple-graph)
      (dag-draw-layout-graph complex-graph)
      
      ;; Generate outputs
      (let ((simple-output (dag-draw-render-ascii simple-graph))
            (complex-output (dag-draw-render-ascii complex-graph)))
        
        ;; Simple output should be compact
        (let ((simple-lines (split-string simple-output "\n")))
          (expect (length simple-lines) :to-be-less-than 25))
        
        ;; Complex output should be larger but not enormous
        (let ((complex-lines (split-string complex-output "\n")))
          (expect (length complex-lines) :to-be-greater-than 10)
          (expect (length complex-lines) :to-be-less-than 50))
        
        ;; Both should show all nodes clearly
        (expect simple-output :to-match "Node A")
        (expect simple-output :to-match "Node B")
        (expect complex-output :to-match "Top")
        (expect complex-output :to-match "Bottom"))))
  
  (it "should handle world coordinate boundaries correctly"
    (let ((graph (dag-draw-create-graph)))
      ;; Add nodes with various positions (some at origin, some offset)
      (dag-draw-add-node graph 'origin "Origin")
      (dag-draw-add-node graph 'offset "Offset")
      
      ;; Manually set coordinates to test boundary handling
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'origin)) 0)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'origin)) 0)
      (setf (dag-draw-node-x-coord (dag-draw-get-node graph 'offset)) 200)
      (setf (dag-draw-node-y-coord (dag-draw-get-node graph 'offset)) 100)
      
      ;; Should render without errors
      (let ((output (dag-draw-render-ascii graph)))
        (expect output :to-be-truthy)
        (expect output :to-match "Origin")
        (expect output :to-match "Offset")))))

;;; dag-draw-coordinate-scaling-test.el ends here